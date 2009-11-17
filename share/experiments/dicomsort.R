#@args [directory]
#@desc Sort a directory containing DICOM files (default ".") into subdirectories by
#@desc series number (as given in DICOM tag 0020,0011). The original files will not be
#@desc deleted or otherwise moved unless DeleteOriginals:true is given.

library(tractor.base)

runExperiment <- function ()
{
    if (nArguments() == 0)
        directory <- "."
    else
        directory <- expandFileName(Arguments[1])
    
    deleteOriginals <- getWithDefault("DeleteOriginals", FALSE)
    
    files <- expandFileName(list.files(directory, full.names=TRUE, recursive=TRUE))
    files <- files[!file.info(files)$isdir]
    nFiles <- length(files)

    count <- 0
    seriesNumbers <- numeric(nFiles)
    
    output(OL$Info, "Reading series numbers from ", nFiles, " files")
    for (i in 1:nFiles)
    {
        metadata <- try(newDicomMetadataFromFile(files[i]), silent=TRUE)
        if (is.null(metadata) || ("try-error" %in% class(metadata)))
        {
            output(OL$Info, "Skipping ", files[i])
            seriesNumbers[i] <- NA
        }
        else
        {
            seriesNumbers[i] <- metadata$getTagValue(0x0020, 0x0011)
            count <- count + 1
            if (count %% 100 == 0)
                output(OL$Verbose, "Done ", count)
        }
    }

    nDicomFiles <- count
    if (nDicomFiles == 0)
        output(OL$Error, "No readable DICOM files were found")

    uniqueSeries <- sort(unique(seriesNumbers))
    output(OL$Info, "Found series ", implode(uniqueSeries,", "), "; creating subdirectories")
    
    for (series in uniqueSeries)
    {
        matchingFiles <- which(seriesNumbers==series)
        if (length(matchingFiles) > 0)
        {
            metadata <- newDicomMetadataFromFile(files[matchingFiles[1]])
            description <- metadata$getTagValue(0x0008, 0x103e)
            output(OL$Info, "Series ", series, " includes ", length(matchingFiles), " files; description is \"", description, "\"")
            
            subdirectory <- paste(series, gsub("\\W","",description,perl=TRUE), sep="_")
            dir.create(file.path(directory, subdirectory))
            success <- file.copy(files[matchingFiles], file.path(directory,subdirectory,basename(files[matchingFiles])))

            if (!all(success))
                output(OL$Warning, "Not all files copied successfully for series ", series, " - nothing will be deleted")
            else if (deleteOriginals)
                unlink(files[matchingFiles])
        }
    }
}
