#@args [directory]
#@desc Sort a directory containing DICOM files (default ".") into subdirectories by series number (as given in DICOM tag 0020,0011). The DICOM files will be copied into the new subdirectories and their file names disambiguated if necessary. The original files will then be deleted to avoid duplicate storage, unless DeleteOriginals:false is given. An additional sort by subject name can also be performed first if required.

library(tractor.base)

runExperiment <- function ()
{
    if (nArguments() == 0)
        directory <- "."
    else
        directory <- expandFileName(Arguments[1])
    
    deleteOriginals <- getConfigVariable("DeleteOriginals", TRUE)
    sortBySubject <- getConfigVariable("SortBySubjectFirst", FALSE)
    
    if (sortBySubject)
        sortDicomDirectory(directory, deleteOriginals, sortOn=c("subject","series"))
    else
        sortDicomDirectory(directory, deleteOriginals, sortOn="series")
}
