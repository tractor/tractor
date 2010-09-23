#@args [directory]
#@desc Sort a directory containing DICOM files (default ".") into subdirectories by series number (as given in DICOM tag 0020,0011). The original files will not be deleted or otherwise moved unless DeleteOriginals:true is given.

library(tractor.base)

runExperiment <- function ()
{
    if (nArguments() == 0)
        directory <- "."
    else
        directory <- expandFileName(Arguments[1])
    
    deleteOriginals <- getWithDefault("DeleteOriginals", FALSE)
    
    sortDicomDirectory(directory, deleteOriginals)
}
