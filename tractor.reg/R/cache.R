withRegistrationCacheLock <- function (expr)
{
    cacheDir <- file.path(tempdir(), "reg-cache")
    lockFileName <- file.path(cacheDir, "lock")
    
    if (!file.exists(cacheDir))
        dir.create(cacheDir)
    
    # Wait for the lock to be released
    while (file.exists(lockFileName))
        Sys.sleep(0.5)
    
    # Grab the lock
    writeLines(as.character(Sys.getpid()), lockFileName)
    
    # Evaluate the original expression
    result <- expr
    
    # Release the lock
    unlink(lockFileName)
    
    return (result)
}

checkRegistrationCache <- function (sourceFileName, targetFileName, method = NULL, transformTypes = NULL, entriesOnly = FALSE)
{
    # Not using thread-safe directory here, because all threads need to see the cache
    cacheIndexFile <- file.path(tempdir(), "reg-cache", "index.txt")
    if (!file.exists(cacheIndexFile))
        return (invisible(NULL))
    
    sourceFileName <- expandFileName(sourceFileName)
    targetFileName <- expandFileName(targetFileName)
    cacheIndex <- read.table(cacheIndexFile, col.names=c("index","source","target","method","types","file"))
    
    toKeep <- cacheIndex$source==sourceFileName & cacheIndex$target==targetFileName
    if (!is.null(method))
        toKeep <- toKeep & cacheIndex$method==method
    if (!is.null(transformTypes))
        toKeep <- toKeep & sapply(lapply(cacheIndex$types,splitAndConvertString,",",fixed=TRUE), function(x) all(transformTypes %in% x))
    
    if (all(!toKeep))
        return (invisible(NULL))
    
    cacheEntries <- subset(cacheIndex, toKeep)
    
    if (entriesOnly)
        return (invisible(cacheEntries))
    else
    {
        report(OL$Info, "Registration cache hit - reusing transform")
        registrationFileName <- as.vector(cacheEntries$file[1])
        registration <- deserialiseReferenceObject(registrationFileName)
        return (invisible(registration))
    }
}

updateRegistrationCache <- function (registration, force = FALSE)
{
    if (!is(registration, "Registration"))
        report(OL$Error, "The specified registration is not a Registration object")
    
    sourceFileName <- registration$getSourceImage()$getSource()
    targetFileName <- registration$getTargetImage()$getSource()
    transformTypeString <- implode(registration$getStoredTransformations(), ",")
    
    # There is potential for race conditions here, so we use a lock
    withRegistrationCacheLock({
        matchingEntries <- checkRegistrationCache(sourceFileName, targetFileName, registration$getMethod(), registration$getStoredTransformations(), entriesOnly=TRUE)
        if (!force && !is.null(matchingEntries) && any(matchingEntries$types==transformTypeString))
            return (FALSE)
    
        cacheDir <- file.path(tempdir(), "reg-cache")
        cacheIndexFile <- file.path(cacheDir, "index.txt")
        registrationFileName <- ensureFileSuffix(tempfile("reg-",cacheDir), "Rdata")
    
        if (file.exists(cacheIndexFile))
            cacheIndex <- read.table(cacheIndexFile, col.names=c("index","source","target","method","types","file"))
        if (!is.null(matchingEntries))
            cacheIndex <- subset(cacheIndex, !(index %in% matchingEntries$index))
        
        registration$serialise(file=registrationFileName)
        cacheEntry <- data.frame(index=max(as.integer(cacheIndex$index))+1L, source=sourceFileName, target=targetFileName, method=registration$getMethod(), types=transformTypeString, file=registrationFileName)
        cacheIndex <- rbind(cacheIndex, cacheEntry)
        write.table(cacheIndex, cacheIndexFile, row.names=FALSE, col.names=FALSE)
        
        return (TRUE)
    })
}
