#@args [directory]
#@desc Sort a directory containing DICOM files (default ".") into subdirectories. By default the files will be sorted by series UID (as given in DICOM tag 0020,000e), but sorting by subject name or scan date can be performed in addition (or instead). The DICOM files will be copied into the new subdirectories and their file names disambiguated if necessary. The original files will then be deleted to avoid duplicate storage, unless DeleteOriginals:false is given.

library(tractor.base)

runExperiment <- function ()
{
    if (nArguments() == 0)
        directory <- "."
    else
        directory <- expandFileName(Arguments[1])
    
    deleteOriginals <- getConfigVariable("DeleteOriginals", TRUE)
    sortOn <- getConfigVariable("SortOn", "series")
    
    sortOn <- splitAndConvertString(sortOn, ",", fixed=TRUE)
    
    sortDicomDirectories(directory, deleteOriginals, sortOn=sortOn)
}
