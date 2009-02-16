.MriSession <- function (.directory)
{
    .directory <- expandFileName(.directory)
    .usesOldBedpost <- FALSE
    if (!file.exists(.directory))
        output(OL$Error, "Session directory does not exist")
    
    .workingDirectory <- file.path(.directory, "tractor")
    if (!file.exists(.workingDirectory) && file.exists(file.path(.directory,"combined")))
        file.rename(file.path(.directory,"combined"), .workingDirectory)
    
    self <- list(
        getBaseDirectory = function () { return (.directory) },
        
        getBedpostDirectory = function ()
        {
            bedpostDir <- file.path(.workingDirectory, "fdt.bedpost")
            bedpostxDir <- file.path(.workingDirectory, "fdt.bedpostX")
            if (file.exists(bedpostDir) && !file.exists(bedpostxDir))
                .usesOldBedpost <<- TRUE
            return (ifelse(.usesOldBedpost, bedpostDir, bedpostxDir))
        },
        
        getCaminoDirectory = function ()
        {
            caminoDir <- file.path(.workingDirectory, "camino")
            if (!file.exists(caminoDir))
                dir.create(caminoDir)
            return (caminoDir)
        },
        
        getImageByType = function (type, fibrePopulation = 1)
        {
            fileName <- self$getImageFileNameByType(type, fibrePopulation)
            if (tolower(type) %in% c("fa","md") && !imageFileExists(fileName))
                runDtifitWithSession(self)
            if (tolower(type) == "lrad" && !imageFileExists(fileName))
                createRadialDiffusivityMapForSession(self)
            return (newMriImageFromFile(fileName))
        },
        
        getImageFileNameByType = function (type, fibrePopulation = 1)
        {
            # The getBedpostDirectory() call must be here because its
            # side-effect (setting .usesOldBedpost) is important
            preBedpostDir <- self$getPreBedpostDirectory()
            bedpostDir <- self$getBedpostDirectory()
            if (.usesOldBedpost)
                fibrePopulation <- NULL
            
            path <- switch (tolower(type),
                            t2=file.path(preBedpostDir, "nodif_brain"),
                            mask=file.path(preBedpostDir, "nodif_brain_mask"),
                            avf=file.path(bedpostDir, paste("mean_f",fibrePopulation,"samples",sep="")),
                            theta=file.path(bedpostDir, paste("merged_th",fibrePopulation,"samples",sep="")),
                            phi=file.path(bedpostDir, paste("merged_ph",fibrePopulation,"samples",sep="")),
                            fa=file.path(preBedpostDir, "dti_FA"),
                            md=file.path(preBedpostDir, "dti_MD"),
                            lax=file.path(preBedpostDir, "dti_L1"),
                            lrad=file.path(preBedpostDir, "dti_Lrad"),
                            NULL)
            
            if (is.null(path))
                output(OL$Error, "Unknown file type (\"", type, "\") specified")
            
            return (path)
        },
        
        getObjectDirectory = function ()
        {
            objectDir <- file.path(.workingDirectory, "objects")
            if (!file.exists(objectDir))
                dir.create(objectDir)
            return (objectDir)
        },
        
        getObjectFileName = function (object)
        {
            return (paste(self$getObjectDirectory(), "/", object, ".Rdata", sep=""))
        },
        
        getPreBedpostDirectory = function ()
        {
            preBedpostDir <- file.path(.workingDirectory, "fdt")
            if (!file.exists(preBedpostDir))
                dir.create(preBedpostDir, showWarnings=FALSE)
            return (preBedpostDir)
        },
        
        getProbtrackDirectory = function ()
        {
            probtrackDir <- file.path(.workingDirectory, "fdt.track")
            if (!file.exists(probtrackDir))
                dir.create(probtrackDir)
            return (probtrackDir)
        },
        
        getWorkingDirectory = function () { return (.workingDirectory) },
        
        isPreprocessed = function () { return (imageFileExists(self$getImageFileNameByType("avf"))) },
        
        nFibres = function ()
        {
            if (!self$isPreprocessed())
                return (NA)
            if (.usesOldBedpost)
                return (1)
            
            # This will give the wrong answer if more than 3 populations were
            # used (but I think Behrens advises against that anyway)
            for (i in 1:4)
            {
                if (!imageFileExists(self$getImageFileNameByType("avf",fibrePopulation=i)))
                    break
            }
            return (i-1)
        },
        
        usesOldBedpost = function ()
        {
            self$getBedpostDirectory()
            return (.usesOldBedpost)
        }
    )
    
    class(self) <- c("session.mri", "list.object", "list")
    invisible (self)
}

isMriSession <- function (object)
{
    return ("session.mri" %in% class(object))
}

deserialiseMriSession <- function (file = NULL, object = NULL)
{
    session <- deserialiseListObject(file, object, .MriSession)
    invisible (session)
}

newSessionFromDirectory <- function (directory, createFiles = FALSE, dicomDir = NULL)
{
    session <- .MriSession(directory)
    if (createFiles)
        createFilesForSession(session, dicomDir)
    invisible (session)
}

createFilesForSession <- function (session, dicomDir = NULL, overwriteQuietly = FALSE)
{
    workingDir <- session$getWorkingDirectory()
    if (file.exists(workingDir))
    {
        if (overwriteQuietly)
            ans <- "y"
        else
            ans <- output(OL$Question, "Internal directory ", workingDir, " exists. This operation will DESTROY it. Continue? [yn]")
        
        if (tolower(ans) != "y")
            return (invisible(NULL))
        else
            unlink(workingDir, recursive=TRUE)
    }
    
    if (is.null(dicomDir))
        dicomDir <- session$getBaseDirectory()
    else if (!(dicomDir %~% "^/"))
        dicomDir <- file.path(session$getBaseDirectory(), dicomDir)
    dicomDir <- gsub("//+", "/", dicomDir, perl=TRUE)
    
    info <- newMriImageFromDicomDirectory(dicomDir, readDiffusionParams=TRUE)
    
    dir.create(workingDir)
    targetDir <- session$getPreBedpostDirectory()
    writeMriImageToFile(info$image, file.path(targetDir,"basic"))
    info$image$summarise()

    if (any(is.na(info$bValues)))
        output(OL$Warning, "Diffusion b-values could not be found in the DICOM files; you need to create a bvals file manually")
    else
        write.table(matrix(info$bValues,nrow=1), file.path(targetDir,"bvals"), row.names=FALSE, col.names=FALSE)

    if (any(is.na(info$bVectors)))
        output(OL$Warning, "Diffusion gradient vectors could not be found in the DICOM files; you need to create a bvecs file manually")
    else
        write.table(info$bVectors, file.path(targetDir,"bvecs"), row.names=FALSE, col.names=FALSE)
}

flipGradientVectorsForSession <- function (session, axes)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    fileName <- file.path(session$getPreBedpostDirectory(), "bvecs")
    bvecs <- as.matrix(read.table(fileName))
    bvecs[axes,] <- (-bvecs[axes,])
    write.table(bvecs, fileName, row.names=FALSE, col.names=FALSE)
}

createCaminoFilesForSession <- function (session, diffusionTime = NULL)
{
    if (!isMriSession(session))
        output(OL$Error, "The specified session is not an MriSession object")
    
    caminoDir <- session$getCaminoDirectory()
    sourceDir <- session$getPreBedpostDirectory()
    if (!file.exists(file.path(sourceDir,"bvals")) || !file.exists(file.path(sourceDir,"bvecs")) || !imageFileExists(file.path(sourceDir,"data")) || !imageFileExists(file.path(sourceDir,"nodif_brain_mask")))
        output(OL$Error, "Some required files are missing - the session must be processed for FSL first")
    
    dir.create(caminoDir)
    
    output(OL$Info, "Creating a scheme file")
    bvals <- unlist(read.table(file.path(sourceDir, "bvals")))
    bvecs <- as.matrix(read.table(file.path(sourceDir, "bvecs")))
    
    if (min(bvals) != 0)
        output(OL$Info, "Minimal b-value in this data set is ", min(bvals), " rather than 0")
    
    zeroes <- which(bvals == min(bvals))
    if (length(zeroes) == 0)
        output(OL$Error, "No b-values are specified for this data set")
    nonzeroes <- setdiff(seq_along(bvals), zeroes)
    volumeOrder <- c(zeroes, nonzeroes)
    
    if (is.null(diffusionTime))
        diffusionTime <- as.numeric(output(OL$Question, "What is the diffusion time for this data set in seconds?"))

    scheme <- c(diffusionTime, length(bvals))
    for (i in volumeOrder)
    {
        modQ <- sqrt(bvals[i] * 1e6 / diffusionTime)
        bvecLength <- vectorLength(bvecs[,i])
        scheme <- c(scheme, bvecs[,i] / ifelse(bvecLength==0,1,bvecLength) * modQ)
    }
    scheme <- round(scheme, 3)
    write.table(as.matrix(scheme), file.path(caminoDir,"sequence.scheme"), row.names=FALSE, col.names=FALSE)
    
    output(OL$Info, "Copying data and mask images")
    dataImage <- newMriImageFromFile(file.path(sourceDir,"data"))
    data <- dataImage[,,,volumeOrder]
    newDataImage <- newMriImageWithData(data, dataImage$getMetadata())
    writeMriImageToCamino(newDataImage, file.path(caminoDir,"data"))
    maskImage <- newMriImageFromFile(file.path(sourceDir,"nodif_brain_mask"))
    writeMriImageToCamino(maskImage, file.path(caminoDir,"mask"))
    
    invisible (NULL)
}

createRadialDiffusivityMapForSession <- function (session)
{
    if (!isMriSession(session))
        output(OL$Error, "The specified session is not an MriSession object")
    
    preBedpostDir <- session$getPreBedpostDirectory()
    
    secondEigenvalue <- newMriImageFromFile(file.path(preBedpostDir, "dti_L2"))
    thirdEigenvalue <- newMriImageFromFile(file.path(preBedpostDir, "dti_L3"))
    radialDiffusivity <- newMriImageWithBinaryFunction(secondEigenvalue, thirdEigenvalue, function(x,y) (x+y)/2)
    
    writeMriImageToFile(radialDiffusivity, session$getImageFileNameByType("lrad"))
}
