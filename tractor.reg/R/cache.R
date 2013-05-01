withTransformationCacheLock <- function (expr)
{
    cacheDir <- file.path(tempdir(), "xfm-cache")
    lockFileName <- file.path(cacheDir, "lock")
    
    if (!file.exists(cacheDir))
        dir.create(cacheDir)
    
    # Wait for the lock to be released
    while (file.exists(lockFileName))
        Sys.sleep(0.5)
    
    # Grab the lock, and release it on exit
    writeLines(as.character(Sys.getpid()), lockFileName)
    on.exit(unlink(lockFileName))
    
    # Evaluate the original expression
    expr
}

checkTransformationCache <- function (sourceFileName, targetFileName, method = NULL, types = NULL, entriesOnly = FALSE)
{
    # Not using thread-safe directory here, because all threads need to see the cache
    cacheIndexFile <- file.path(tempdir(), "xfm-cache", "index.txt")
    if (!file.exists(cacheIndexFile))
        return (invisible(NULL))
    
    sourceFileName <- expandFileName(sourceFileName)
    targetFileName <- expandFileName(targetFileName)
    
    if (isTemporaryFile(sourceFileName) || isTemporaryFile(targetFileName))
        return (invisible(NULL))
    
    cacheIndex <- read.table(cacheIndexFile, col.names=c("index","source","target","method","types","file"), stringsAsFactors=FALSE)
    
    toKeep <- cacheIndex$source==sourceFileName & cacheIndex$target==targetFileName
    if (!is.null(method))
        toKeep <- toKeep & cacheIndex$method==method
    if (!is.null(types))
        toKeep <- toKeep & sapply(cacheIndex$types, function(x) all(types %in% unlist(strsplit(x,",",fixed=TRUE))))
    
    if (all(!toKeep))
        return (invisible(NULL))
    
    cacheEntries <- subset(cacheIndex, toKeep)
    
    if (entriesOnly)
        return (invisible(cacheEntries))
    else
    {
        report(OL$Info, "Registration cache hit - reusing transform")
        transformFileName <- as.vector(cacheEntries$file[1])
        transform <- deserialiseReferenceObject(transformFileName)
        return (invisible(transform))
    }
}

updateTransformationCache <- function (transform, force = FALSE)
{
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified transform is not a Transformation object")
    
    sourceFileName <- transform$getSourceImage()$getSource()
    targetFileName <- transform$getTargetImage()$getSource()
    transformTypeString <- implode(transform$getTypes(), ",")
    
    # There is potential for race conditions here, so we use a lock
    withTransformationCacheLock({
        matchingEntries <- checkTransformationCache(sourceFileName, targetFileName, transform$getMethod(), transform$getTypes(), entriesOnly=TRUE)
        if (!force && !is.null(matchingEntries) && any(matchingEntries$types==transformTypeString))
            return (FALSE)
    
        cacheDir <- file.path(tempdir(), "xfm-cache")
        cacheIndexFile <- file.path(cacheDir, "index.txt")
        transformFileName <- ensureFileSuffix(tempfile("reg-",cacheDir), "Rdata")
    
        if (file.exists(cacheIndexFile))
        {
            cacheIndex <- read.table(cacheIndexFile, col.names=c("index","source","target","method","types","file"), stringsAsFactors=FALSE)
            index <- max(as.integer(cacheIndex$index)) + 1L
        }
        else
        {
            cacheIndex <- NULL
            index <- 1L
        }
        
        if (!is.null(matchingEntries))
        {
            cacheIndex <- subset(cacheIndex, !(cacheIndex$index %in% matchingEntries$index))
            unlink(matchingEntries$file)
        }
        
        transform$serialise(file=transformFileName)
        cacheEntry <- data.frame(index=index, source=sourceFileName, target=targetFileName, method=transform$getMethod(), types=transformTypeString, file=transformFileName)
        cacheIndex <- rbind(cacheIndex, cacheEntry)
        write.table(cacheIndex, cacheIndexFile, row.names=FALSE, col.names=FALSE)
        
        return (TRUE)
    })
}
