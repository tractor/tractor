gradientDirectionsAvailableForSession <- function (session)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    paths <- file.path(session$getPreBedpostDirectory(), c("bvals","bvecs"))
    return (all(file.exists(paths)))
}

saveSeriesDescriptionsForSession <- function (session, descriptions)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    descriptionString <- implode(gsub("\\W","",descriptions,perl=TRUE), ",")
    writeLines(descriptionString, file.path(session$getPreBedpostDirectory(),"descriptions.txt"))
}

checkGradientCacheForSession <- function (session)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    descriptionsFile <- file.path(session$getPreBedpostDirectory(), "descriptions.txt")
    if (!file.exists(descriptionsFile))
        return (NULL)
    seriesDescriptions <- readLines(descriptionsFile, 1)
    
    cacheDirectory <- file.path(Sys.getenv("HOME"), ".tractor", "gradient-cache")
    cacheIndexFile <- file.path(cacheDirectory, "index.txt")
    if (!file.exists(cacheIndexFile))
        return (NULL)
    
    cacheIndex <- read.table(cacheIndexFile, col.names=c("descriptions","number"))
    cacheEntry <- subset(cacheIndex, descriptions==seriesDescriptions)
    
    if (nrow(cacheEntry) != 1)
        return (NULL)
    
    gradientSet <- as.matrix(read.table(file.path(cacheDirectory, paste("set",cacheEntry$number,".txt",sep=""))))
    return (gradientSet)
}

updateGradientCacheFromSession <- function (session)
{
    if (!gradientDirectionsAvailableForSession(session))
        return (FALSE)
    if (!is.null(checkGradientCacheForSession(session)))
        return (FALSE)
    
    preBedpostDirectory <- session$getPreBedpostDirectory()
    
    descriptionsFile <- file.path(preBedpostDirectory, "descriptions.txt")
    if (!file.exists(descriptionsFile))
        return (FALSE)
    seriesDescriptions <- readLines(descriptionsFile, 1)
    
    cacheDirectory <- file.path(Sys.getenv("HOME"), ".tractor", "gradient-cache")
    if (!file.exists(cacheDirectory))
        dir.create(cacheDirectory)
    
    cacheIndexFile <- file.path(cacheDirectory, "index.txt")
    if (!file.exists(cacheIndexFile))
    {
        cacheIndex <- NULL
        number <- 1
    }
    else
    {
        cacheIndex <- read.table(cacheIndexFile, col.names=c("descriptions","number"))
        number <- max(cacheIndex$number) + 1
    }
    
    bvecs <- as.matrix(read.table(file.path(preBedpostDirectory, "bvecs")))
    bvecLengths <- apply(bvecs, 2, vectorLength)
    bvecMatrix <- t(bvecs) / ifelse(bvecLengths==0, 1, bvecLengths)
    
    bvals <- unlist(read.table(file.path(preBedpostDirectory, "bvals")))
    gradientSet <- cbind(bvecMatrix, bvals)
    write.table(gradientSet, file.path(cacheDirectory,paste("set",number,".txt",sep="")), row.names=FALSE, col.names=FALSE)
    
    cacheIndex <- rbind(cacheIndex, data.frame(descriptions=seriesDescriptions,number=number))
    write.table(cacheIndex, cacheIndexFile, row.names=FALSE, col.names=FALSE)

    return (TRUE)
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

rotateGradientVectorsForSession <- function (session)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    transforms <- readEddyCorrectTransformsForSession(session)
    decompositions <- lapply(transforms, decomposeAffineTransform3D)
    
    fileName <- file.path(session$getPreBedpostDirectory(), "bvecs")
    bvecs <- as.matrix(read.table(fileName))
    bvecs <- sapply(1:ncol(bvecs), function (i) decompositions[[i]]$rotationMatrix %*% bvecs[,i])
    write.table(bvecs, fileName, row.names=FALSE, col.names=FALSE)
}
