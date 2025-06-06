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
    if (is.null(session$getDiffusionScheme()))
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
    
    scheme <- session$getDiffusionScheme(unrotated=TRUE)
    gradientSet <- cbind(scheme$getGradientDirections(), scheme$getBValues())
    write.table(gradientSet, file.path(cacheDirectory,paste("set",number,".txt",sep="")), row.names=FALSE, col.names=FALSE)
    
    cacheIndex <- rbind(cacheIndex, data.frame(descriptions=seriesDescriptions,number=number))
    write.table(cacheIndex, cacheIndexFile, row.names=FALSE, col.names=FALSE)

    return (TRUE)
}

flipGradientVectorsForSession <- function (session, axes, unrotated = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    scheme <- session$getDiffusionScheme(unrotated=unrotated)
    directions <- scheme$getGradientDirections()
    directions[,axes] <- (-directions[,axes])
    scheme <- asDiffusionScheme(directions, scheme$getBValues())
    session$updateDiffusionScheme(scheme, unrotated=unrotated)
}

rotateGradientVectorsForSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    transformSets <- getVolumeTransformationForSession(session,"diffusion")$getTransformSets()
    decompositions <- lapply(transformSets, function(set) RNiftyReg::decomposeAffine(set$getObject("affine")))
    
    unrotatedScheme <- session$getDiffusionScheme(unrotated=TRUE)
    directions <- unrotatedScheme$getGradientDirections()
    directions <- sapply(1:nrow(directions), function(i) decompositions[[i]]$rotationMatrix %*% directions[i,])
    rotatedScheme <- asDiffusionScheme(t(directions), unrotatedScheme$getBValues())
    session$updateDiffusionScheme(unrotatedScheme, unrotated=TRUE)
    session$updateDiffusionScheme(rotatedScheme)
}
