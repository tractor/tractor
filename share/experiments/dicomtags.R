#@args [file name]
#@desc Produce a full list of all the DICOM tags contained in the specified file. If the name given corresponds to a directory then the tags for the first DICOM file found in that directory will be shown. If the file name is missing the current directory will be used. Descriptions of the tag, rather than its DICOM group and element numbers, will be given unless Descriptions:false is specified. Siemens ASCII fields will be printed instead of the main DICOM tags if SiemensAscii is true.
#@nohistory TRUE

library(tractor.base)

runExperiment <- function ()
{
    descriptions <- getConfigVariable("Descriptions", TRUE)
    showAscii <- getConfigVariable("SiemensAscii", FALSE)
    
    if (nArguments() > 0)
        fileName <- implode(Arguments, sep=" ")
    else
        fileName <- "."
    
    if (!file.exists(fileName))
        report(OL$Error, "Specified DICOM file or directory \"", fileName, "\" does not exist")
    
    if (file.info(fileName)$isdir)
    {
        allFileNames <- list.files(fileName, full.names=TRUE, recursive=TRUE)
        allFileNames <- allFileNames[!file.info(allFileNames)$isdir]
        for (currentFileName in allFileNames)
        {
            metadata <- readDicomFile(currentFileName, ascii=showAscii)
            if (!is.null(metadata))
            {
                report(OL$Info, "Displaying tags for file \"", currentFileName, "\"")
                break
            }
        }
        if (!exists("metadata") || is.null(metadata))
            report(OL$Error, "The directory \"", fileName, "\" appears to contain no readable DICOM files")
    }
    else
    {
        metadata <- readDicomFile(fileName, ascii=showAscii)
        if (is.null(metadata))
            report(OL$Error, "The file \"", fileName, "\" appears not to be a DICOM file or is unreadable")
    }
    
    if (showAscii)
        cat(metadata$getAsciiFields(), sep="\n")
    else
        print(metadata, descriptions=descriptions)
}
