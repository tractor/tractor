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
    cacheEntry <- subset(cacheIndex, cacheIndex$descriptions==seriesDescriptions)
    
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
        cacheEntry <- subset(cacheIndex, cacheIndex$descriptions==seriesDescriptions)
        if (nrow(cacheEntry) == 0)
            number <- max(cacheIndex$number) + 1
        else if (!force)
            return (FALSE)
        else
        {
            cacheIndex <- subset(cacheIndex, cacheIndex$descriptions!=seriesDescriptions)
            number <- cacheEntry$number
        }
    }
    
    scheme <- newSimpleDiffusionSchemeFromSession(session)
    gradientSet <- cbind(scheme$getGradientDirections(), scheme$getBValues())
    write.table(gradientSet, file.path(cacheDirectory,paste("set",number,".txt",sep="")), row.names=FALSE, col.names=FALSE)
    
    cacheIndex <- rbind(cacheIndex, data.frame(descriptions=seriesDescriptions,number=number))
    write.table(cacheIndex, cacheIndexFile, row.names=FALSE, col.names=FALSE)

    return (TRUE)
}

flipGradientVectorsForSession <- function (session, axes)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    scheme <- newSimpleDiffusionSchemeFromSession(session)
    directions <- scheme$getGradientDirections()
    directions[,axes] <- (-directions[,axes])
    scheme <- SimpleDiffusionScheme$new(scheme$getBValues(), directions)
    writeSimpleDiffusionSchemeForSession(session, scheme)
}

rotateGradientVectorsForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    transform <- getVolumeTransformationForSession(session, "diffusion")
    decompositions <- tractor.reg::decomposeTransformation(transform)
    
    unrotatedScheme <- newSimpleDiffusionSchemeFromSession(session, unrotated=TRUE)
    directions <- unrotatedScheme$getGradientDirections()
    directions <- sapply(1:nrow(directions), function(i) decompositions[[i]]$rotationMatrix %*% directions[i,])
    rotatedScheme <- SimpleDiffusionScheme$new(unrotatedScheme$getBValues(), t(directions))
    writeSimpleDiffusionSchemeForSession(session, unrotatedScheme, unrotated=TRUE)
    writeSimpleDiffusionSchemeForSession(session, rotatedScheme)
}
