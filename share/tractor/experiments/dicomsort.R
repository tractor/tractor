#@args [directory]
#@desc Sort a directory containing DICOM files (default ".") into subdirectories. By default the files will be sorted by series UID (as given in DICOM tag 0020,000e), but sorting by subject name or scan date can be performed in addition (or instead). The DICOM files will be copied into the new subdirectories (within the old directories unless Nested:false is given), and their file names disambiguated if necessary. The original files will then be deleted to avoid duplicate storage, unless DeleteOriginals:false is given. The newer "divest" back-end is based on the well-established and more robust "dcm2niix" tool; in this case the SortOn and SeriesIdentifier options are ignored.

library(tractor.base)

runExperiment <- function ()
{
    if (nArguments() == 0)
        directory <- "."
    else
        directory <- expandFileName(Arguments[1])
    
    method <- getConfigVariable("Method", "internal", "character", validValues=c("internal","divest"))
    deleteOriginals <- getConfigVariable("DeleteOriginals", TRUE)
    sortOn <- getConfigVariable("SortOn", "series")
    seriesId <- getConfigVariable("SeriesIdentifier", "UID", validValues=c("UID","number","time"))
    forceStack <- getConfigVariable("AlwaysStack", FALSE)
    nested <- getConfigVariable("Nested", TRUE)
    cleanEmpty <- getConfigVariable("CleanEmpty", TRUE)
    
    if (method == "internal")
    {
        sortOn <- splitAndConvertString(sortOn, ",", fixed=TRUE)
        sortDicomDirectories(directory, deleteOriginals, sortOn=sortOn, seriesId=seriesId, nested=nested)
    }
    else if (method == "divest")
    {
        divestVerbosity <- switch(names(getOutputLevel()), Debug=2L, Verbose=0L, -1L)
        divest::sortDicom(directory, forceStack=forceStack, verbosity=divestVerbosity, labelFormat="%n_%t_S%3s_%d", nested=nested, keepUnsorted=!deleteOriginals)
    }
    
    if (cleanEmpty)
    {
        cleanup <- function (d)
        {
            files <- list.files(d, all.files=TRUE, full.names=TRUE, recursive=FALSE, no..=TRUE)
            dirs <- file.info(files)$isdir
            empty <- sapply(files[dirs], cleanup)
            
            # Delete if the directory is empty, or only contains empty directories
            if (length(files) == 0 || (all(dirs) && all(empty)))
            {
                report(OL$Verbose, "Deleting empty directory #{d}")
                unlink(d, recursive=TRUE)
                return (TRUE)
            }
            else
                return (FALSE)
        }
        
        cleanup(directory)
    }
}
