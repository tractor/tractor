#@args session directory, target directory
#@desc Create a clone of a session hierarchy under the specified target directory. The entire "tractor" subdirectory, but nothing else, will be copied across. This will anonymise the hierarchy if DICOM files are stored outside the "tractor" subdirectory and no Analyze/NIfTI volume headers contain private data. The operation is a move rather than a copy with DeleteOriginal:true. The target directory will be created if it doesn't exist.
#@interactive TRUE

library(tractor.base)
library(tractor.session)

runExperiment <- function ()
{
    recursiveDirectoryCopy <- function (from, to, all.files = FALSE)
    {
        sourceFiles <- list.files(from, all.files=all.files, full.names=TRUE)
        sourceFiles <- sourceFiles[!(basename(sourceFiles) %in% c(".",".."))]
        
        output(OL$Info, "Creating directory ", to)
        success <- dir.create(to)
        targetFiles <- file.path(to, basename(sourceFiles))
        
        for (i in seq_along(sourceFiles))
        {            
            if (file.info(sourceFiles[i])$isdir)
                success <- success && recursiveDirectoryCopy(sourceFiles[i], targetFiles[i], all.files=all.files)
            else
            {
                output(OL$Verbose, "Copying file ", targetFiles[i])
                success <- success && file.copy(sourceFiles[i], targetFiles[i], overwrite=TRUE)
            }
        }
        
        return (success)
    }
    
    requireArguments("session directory", "target directory")
    
    deleteOriginal <- getWithDefault("DeleteOriginal", FALSE, errorIfInvalid=TRUE)
    copyHidden <- getWithDefault("CopyHidden", FALSE)
    
    session <- newSessionFromDirectory(Arguments[1])
    
    targetDir <- Arguments[2]
    if (file.exists(targetDir))
    {
        if (!file.info(targetDir)$isdir)
            output(OL$Error, "Target is not a directory")
        
        targetSessionDir <- file.path(targetDir, basename(session$getBaseDirectory()))
        
        if (file.exists(targetSessionDir))
        {
            ans <- output(OL$Question, "Directory ", targetSessionDir, " already exists - delete it? [yn]")
            if (tolower(ans) == "y")
                unlink(targetSessionDir, recursive=TRUE)
            else
                return (invisible(NULL))
        }
    }
    else
        targetSessionDir <- targetDir
    
    success <- dir.create(targetSessionDir, showWarnings=FALSE)
    if (!success)
        output(OL$Error, "Could not create new session directory")
    
    success <- recursiveDirectoryCopy(session$getWorkingDirectory(), file.path(targetSessionDir,basename(session$getWorkingDirectory())), all.files=copyHidden)
    if (!success)
        output(OL$Warning, "Not all files copied successfully - nothing will be deleted")
    else if (deleteOriginal)
        unlink(session$getWorkingDirectory(), recursive=TRUE)
    
    return (invisible(NULL))
}
