#@args session directory, target directory
#@desc Create a clone of a session hierarchy under the specified target directory. The entire "tractor" subdirectory, but nothing else, will be copied across. This will anonymise the hierarchy if DICOM files are stored outside the "tractor" subdirectory and no Analyze/NIfTI/MGH volume headers contain private data. The operation is a move rather than a copy with DeleteOriginal:true. The target directory will be created if it doesn't exist.
#@interactive TRUE

library(tractor.base)
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "target directory")
    
    map <- getConfigVariable("Map", FALSE)
    deleteOriginal <- getConfigVariable("DeleteOriginal", FALSE, errorIfInvalid=TRUE)
    copyHidden <- getConfigVariable("CopyHidden", FALSE)
    
    session <- attachMriSession(Arguments[1])
    
    targetDir <- Arguments[2]
    if (file.exists(targetDir))
    {
        if (!file.info(targetDir)$isdir)
            report(OL$Error, "Target is not a directory")
        
        targetSessionDir <- file.path(targetDir, basename(session$getDirectory()))
        
        if (file.exists(targetSessionDir))
        {
            if (ask("Directory #{targetSessionDir} already exists - delete it? [yn]", valid=c("y","n")) == "y")
                unlink(targetSessionDir, recursive=TRUE)
            else
                return (invisible(NULL))
        }
    }
    else
        targetSessionDir <- targetDir
    
    targetRoot <- expandFileName(file.path(targetSessionDir, basename(session$getDirectory("root"))))
    if (map)
    {
        success <- dir.create(targetRoot, recursive=TRUE, showWarnings=FALSE)
        assert(success, "Could not create new session directory")
        
        sourceMap <- session$getMap()
        targetMap <- list()
        for (place in names(sourceMap))
        {
            sourcePath <- expandFileName(sourceMap[[place]], base=session$getDirectory("root"))
            if (isTRUE(file.info(sourcePath)$isdir))
            {
                targetPath <- relativePath(sourcePath, file.path(targetRoot,"map.yaml"))
                targetMap[[place]] <- targetPath
            }
        }
        
        if (length(targetMap) > 0)
            yaml::write_yaml(targetMap, file.path(targetRoot,"map.yaml"))
    }
    else
    {
        success <- dir.create(targetSessionDir, showWarnings=FALSE)
        assert(success, "Could not create new session directory")
        
        success <- copyDirectory(session$getDirectory("root"), targetRoot, allFiles=copyHidden, deleteOriginal=deleteOriginal)
        assert(success, "Directory copy failed")
    }
    
    return (invisible(NULL))
}
