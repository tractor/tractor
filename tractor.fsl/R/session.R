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
        
        getImageByType = function (type, fibrePopulation = 1)
        {
            fileName <- self$getImageFileNameByType(type, fibrePopulation)
            if (type %in% c("fa","md") && !imageFileExists(fileName))
                runDtifitWithSession(self)
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
            
            if (type == "t2")
                return (file.path(preBedpostDir, "nodif_brain"))
            else if (type == "mask")
                return (file.path(preBedpostDir, "nodif_brain_mask"))
            else if (type == "avf")
                return (file.path(bedpostDir, paste("mean_f",fibrePopulation,"samples",sep="")))
            else if (type == "theta")
                return (file.path(bedpostDir, paste("merged_th",fibrePopulation,"samples",sep="")))
            else if (type == "phi")
                return (file.path(bedpostDir, paste("merged_ph",fibrePopulation,"samples",sep="")))
            else if (type == "fa")
                return (file.path(preBedpostDir, "dti_FA"))
            else if (type == "md")
                return (file.path(preBedpostDir, "dti_MD"))
            else
                output(OL$Error, "Unknown file type (", type, ") specified")
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
