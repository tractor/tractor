MriSession <- setRefClass("MriSession", contains="SerialisableObject", fields=list(directory="character"), methods=list(
    initialize = function (directory = NULL)
    {
        if (is.null(directory))
            return (initFields(directory=""))
        else if (length(directory) != 1)
            report(OL$Error, "Session directory name should have length 1")
        else if (!file.exists(directory))
            report(OL$Error, "Session directory does not exist")
        else
        {
            object <- initFields(directory=expandFileName(directory))
            updateSessionHierarchy(object)
            return (object)
        }
    },
    
    getDirectory = function (type = NULL, createIfMissing = FALSE)
    {
        if (is.null(type))
            return (directory)
        else
        {
            requiredDir <- switch(type, root="tractor",
                                        diffusion=file.path("tractor","diffusion"),
                                        camino=file.path("tractor","camino"),
                                        fdt=file.path("tractor","fdt"),
                                        bedpost=file.path("tractor","fdt.bedpostX"),
                                        probtrack=file.path("tractor","fdt.track"),
                                        "")
            if (requiredDir == "")
                report(OL$Error, "Directory type \"", type, "\" is not valid")
            else
            {
                requiredDir <- file.path(directory, requiredDir)
                if (createIfMissing && !file.exists(requiredDir))
                    dir.create(requiredDir, recursive=TRUE)
                return (requiredDir)
            }
        }
    },
    
    getImageByType = function (type, place = NULL, index = 1)
    {
        fileName <- getImageFileNameByType(type, place, index)
        if (tolower(type) %in% c("fa","md","lax") && !imageFileExists(fileName))
            runDtifitWithSession(.self)
        if (tolower(type) == "lrad" && !imageFileExists(fileName))
            createRadialDiffusivityMapForSession(.self)
        return (newMriImageFromFile(fileName))
    },
    
    getImageFileNameByType = function (type, place = NULL, index = 1) { return (getImageFileNameForSession(.self, type, place, index)) },
    
    getObjectDirectory = function ()
    {
        objectDir <- file.path(getDirectory("root"), "objects")
        if (!file.exists(objectDir))
            dir.create(objectDir, recursive=TRUE)
        return (objectDir)
    },
    
    getObjectFileName = function (object) { return (file.path(getObjectDirectory(), ensureFileSuffix(object,"Rdata"))) },
    
    isPreprocessed = function () { return (imageFileExists(getImageFileNameByType("avf"))) },
    
    nFibres = function ()
    {
        if (!isPreprocessed())
            return (NA)
        
        i <- 1
        while (imageFileExists(getImageFileNameByType("avf",index=i)))
            i <- i + 1

        return (i-1)
    }
))

newSessionFromDirectory <- function (directory, createFiles = FALSE, dicomDir = NULL)
{
    session <- MriSession$new(directory)
    if (createFiles)
        createFilesForSession(session, dicomDir)
    invisible (session)
}

getImageFileNameForSession <- function (session, type, place = NULL, index = 1)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    type <- tolower(type)
    
    if (is.null(place))
    {
        locs <- sapply(.DefaultSessionMap, function(x) type %in% names(x))
        if (sum(locs) > 1)
            report(OL$Error, "The specified file type (\"", type, "\") can be present in multiple places")
        else if (sum(locs) < 1)
            report(OL$Error, "The specified file type (\"", type, "\") does not have a standard location")
        else
            place <- names(.DefaultSessionMap)[locs]
    }
    
    map <- .DefaultSessionMap[[place]]
    directory <- session$getDirectory(place)
    if (file.exists(file.path(directory, "map.yaml")))
    {
        map <- c(readYaml(file.path(directory, "map.yaml")), map)
        map <- map[!duplicated(names(map))]
    }
    
    names(map) <- tolower(names(map))
    
    fileName <- map[[type]]
    if (is.null(fileName))
        report(OL$Error, "Image type \"", type, "\" is not valid")
    
    if (fileName %~% "\\%")
    {
        if (length(index) > 1)
            fileName <- sapply(1:3, function(i) sub("%",as.character(i),fileName,fixed=TRUE))
        else
            fileName <- sub("%", as.character(index), fileName, fixed=TRUE)
    }
    
    filePath <- file.path(directory, fileName)
    
    return (filePath)
}

updateSessionHierarchy <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    oldFdtDirectory <- file.path(session$getDirectory("root"), "fdt")
    diffusionDirectory <- session$getDirectory("diffusion")
    
    if (file.exists(oldFdtDirectory) && !file.exists(diffusionDirectory))
    {
        # Assume this is a TractoR 1.x session directory: rename the FDT
        # directory, insert the appropriate map, and move FDT-only files back
        file.rename(oldFdtDirectory, diffusionDirectory)
        writeYaml(.FdtDiffusionMap, file.path(diffusionDirectory, "map.yaml"), capitaliseLabels=FALSE)
        
        filesToMoveBack <- c("bvals", "bvecs", "data.ecclog")
        filesToMoveBack <- filesToMoveBack[file.exists(file.path(diffusionDirectory,filesToMoveBack))]
        if (length(filesToMoveBack) > 0)
        {
            newFdtDirectory <- session$getDirectory("fdt", createIfMissing=TRUE)
            file.rename(file.path(diffusionDirectory,filesToMoveBack), file.path(newFdtDirectory,filesToMoveBack))
        }
    }
}

standardiseSessionHierarchy <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    for (dirName in names(.DefaultSessionMap))
    {
        directory <- session$getDirectory(dirName)
        if (file.exists(file.path(directory, "map.yaml")))
        {
            map <- readYaml(file.path(directory, "map.yaml"))
            names <- intersect(names(map), names(.DefaultSessionMap[[dirName]]))
            files <- file.path(directory, unlist(map[names]))
            
            nonImageFiles <- unlist(.DefaultSessionMap[[dirName]][names]) %~% "\\."
            wildcardFiles <- unlist(.DefaultSessionMap[[dirName]][names]) %~% "\\%"
            otherFiles <- !(nonImageFiles | wildcardFiles)
            
            if (any(nonImageFiles))
            {
                fileExistence <- file.exists(files[nonImageFiles])
                currentNames <- names[nonImageFiles][fileExistence]
                currentFiles <- files[nonImageFiles][fileExistence]
                file.rename(currentFiles, file.path(directory,unlist(.DefaultSessionMap[[dirName]][currentNames])))
            }
            if (any(wildcardFiles))
            {
                # Wildcard files are assumed to be images
                for (i in 1:9)
                {
                    currentFiles <- sub("\\%", i, files[wildcardFiles], perl=TRUE)
                    fileExistence <- imageFileExists(currentFiles)
                    currentNames <- names[wildcardFiles][fileExistence]
                    currentFiles <- currentFiles[fileExistence]
                    currentTargets <- sub("\\%", i, unlist(.DefaultSessionMap[[dirName]][currentNames]), perl=TRUE)
                    copyImageFiles(currentFiles, file.path(directory,currentTargets), deleteOriginals=TRUE)
                }
            }
            if (any(otherFiles))
            {
                fileExistence <- imageFileExists(files[otherFiles])
                currentNames <- names[otherFiles][fileExistence]
                currentFiles <- files[otherFiles][fileExistence]
                copyImageFiles(currentFiles, file.path(directory,unlist(.DefaultSessionMap[[dirName]][currentNames])), deleteOriginals=TRUE)
            }
            
            unlink(file.path(directory, "map.yaml"))
        }
    }
}

createFilesForSession <- function (session, dicomDir = NULL, overwriteQuietly = FALSE)
{
    workingDir <- session$getDirectory("root")
    if (file.exists(workingDir))
    {
        if (overwriteQuietly)
            ans <- "y"
        else
            ans <- report(OL$Question, "Internal directory ", workingDir, " exists. This operation will DESTROY it. Continue? [yn]")
        
        if (tolower(ans) != "y")
            return (invisible(NULL))
        else
            unlink(workingDir, recursive=TRUE)
    }
    
    if (is.null(dicomDir))
        dicomDir <- session$getDirectory()
    else if (!(dicomDir %~% "^/"))
        dicomDir <- file.path(session$getDirectory(), dicomDir)
    dicomDir <- gsub("//+", "/", dicomDir, perl=TRUE)
    
    info <- newMriImageFromDicomDirectory(dicomDir, readDiffusionParams=TRUE)
    
    session$getDirectory("diffusion", createIfMissing=TRUE)
    writeMriImageToFile(info$image, session$getImageFileNameByType("rawdata","diffusion"))
    info$image$summarise()
    
    seriesDescriptions <- implode(gsub("\\W","",info$seriesDescriptions,perl=TRUE), ",")
    writeLines(seriesDescriptions, file.path(session$getDirectory("diffusion"),"descriptions.txt"))

    if (all(!is.na(info$bValues)) && all(!is.na(info$bVectors)))
    {
        scheme <- newSimpleDiffusionSchemeWithDirections(info$bVectors, info$bValues)
        writeSimpleDiffusionSchemeForSession(session, scheme)
    }
}

createCaminoFilesForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    caminoDir <- session$getDirectory("camino")
    if (file.exists(caminoDir))
    {
        ans <- report(OL$Question, "Internal directory ", caminoDir, " exists. This operation will DESTROY it. Continue? [yn]")
        if (tolower(ans) != "y")
            return (invisible(NULL))
        else
            unlink(caminoDir, recursive=TRUE)
    }
    
    sourceDir <- session$getDirectory("fdt")
    if (!file.exists(file.path(sourceDir,"bvals")) || !file.exists(file.path(sourceDir,"bvecs")) || !imageFileExists(session$getImageFileNameByType("data","diffusion")) || !imageFileExists(session$getImageFileNameByType("mask","diffusion")))
        report(OL$Error, "Some required files are missing - the session must be processed for FSL first")
    
    dir.create(caminoDir)
    
    # Having created the Camino directory, reading and writing back the
    # scheme is sufficient to create the Camino file
    report(OL$Info, "Creating a scheme file")
    scheme <- newSimpleDiffusionSchemeFromSession(session)
    writeSimpleDiffusionSchemeForSession(session, scheme)
    
    report(OL$Info, "Copying data and mask images")
    dataImage <- session$getImageByType("data", "diffusion")
    writeMriImageToCamino(dataImage, file.path(caminoDir,"data"))
    maskImage <- session$getImageByType("mask", "diffusion")
    writeMriImageToCamino(maskImage, file.path(caminoDir,"mask"))
    
    invisible (NULL)
}

createRadialDiffusivityMapForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "The specified session is not an MriSession object")
    
    secondEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=2)
    thirdEigenvalue <- session$getImageByType("eigenvalue", "diffusion", index=3)
    radialDiffusivity <- newMriImageWithBinaryFunction(secondEigenvalue, thirdEigenvalue, function(x,y) (x+y)/2)
    
    writeMriImageToFile(radialDiffusivity, session$getImageFileNameByType("radialdiff"))
}
