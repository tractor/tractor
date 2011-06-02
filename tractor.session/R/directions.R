gradientDirectionsAvailableForSession <- function (session)
{
    return (!is.null(newSimpleDiffusionSchemeFromSession(session)))
}

saveSeriesDescriptionsForSession <- function (session, descriptions)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    descriptionString <- implode(gsub("\\W","",descriptions,perl=TRUE), ",")
    writeLines(descriptionString, file.path(session$getDirectory("diffusion"),"descriptions.txt"))
}

checkGradientCacheForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    descriptionsFile <- file.path(session$getDirectory("diffusion"), "descriptions.txt")
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

updateGradientCacheFromSession <- function (session, force = FALSE)
{
    if (!gradientDirectionsAvailableForSession(session))
        return (FALSE)
    
    descriptionsFile <- file.path(session$getDirectory("diffusion"), "descriptions.txt")
    if (!file.exists(descriptionsFile))
        return (FALSE)
    seriesDescriptions <- readLines(descriptionsFile, 1)
    
    cacheDirectory <- file.path(Sys.getenv("HOME"), ".tractor", "gradient-cache")
    if (!file.exists(cacheDirectory))
        dir.create(cacheDirectory, recursive=TRUE)
    
    cacheIndexFile <- file.path(cacheDirectory, "index.txt")
    if (!file.exists(cacheIndexFile))
    {
        cacheIndex <- NULL
        number <- 1
    }
    else
    {
        cacheIndex <- read.table(cacheIndexFile, col.names=c("descriptions","number"))
        cacheEntry <- subset(cacheIndex, descriptions==seriesDescriptions)
        if (nrow(cacheEntry) == 0)
            number <- max(cacheIndex$number) + 1
        else if (!force)
            return (FALSE)
        else
        {
            cacheIndex <- subset(cacheIndex, descriptions!=seriesDescriptions)
            number <- cacheEntry$number
        }
    }
    
    schemeComponents <- newSimpleDiffusionSchemeFromSession(session)$expandComponents()
    gradientSet <- cbind(t(schemeComponents$directions), schemeComponents$bValues)
    write.table(gradientSet, file.path(cacheDirectory,paste("set",number,".txt",sep="")), row.names=FALSE, col.names=FALSE)
    
    cacheIndex <- rbind(cacheIndex, data.frame(descriptions=seriesDescriptions,number=number))
    write.table(cacheIndex, cacheIndexFile, row.names=FALSE, col.names=FALSE)

    return (TRUE)
}

flipGradientVectorsForSession <- function (session, axes)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    schemeComponents <- newSimpleDiffusionSchemeFromSession(session)$expandComponents()
    schemeComponents$directions[axes,] <- (-schemeComponents$directions[axes,])
    scheme <- newSimpleDiffusionSchemeWithDirections(schemeComponents$directions, schemeComponents$bValues)
    writeSimpleDiffusionSchemeForSession(session, scheme)
}

rotateGradientVectorsForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    transforms <- readEddyCorrectTransformsForSession(session)
    decompositions <- lapply(transforms, decomposeAffineTransform3D)
    
    schemeComponents <- newSimpleDiffusionSchemeFromSession(session)$expandComponents()
    schemeComponents$directions <- sapply(1:ncol(schemeComponents$directions), function (i) decompositions[[i]]$rotationMatrix %*% schemeComponents$directions[,i])
    scheme <- newSimpleDiffusionSchemeWithDirections(schemeComponents$directions, schemeComponents$bValues)
    writeSimpleDiffusionSchemeForSession(session, scheme)
}
