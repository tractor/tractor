#@args image file(s), [format]
#@desc Reads and rewrites the specified Analyze/NIfTI/MGH image file(s) using the specified file format. Note that the original file will be REPLACED. The format can be "ANALYZE" (for Analyze format), "NIFTI" (for single-file NIfTI), "NIFTI_PAIR" (NIfTI header/image pair), "MGH" (for MGH format), or any of these with "_GZ" appended for the equivalent formats compressed with gzip(1). If the format is not specified, it will be taken from the TRACTOR_FILETYPE environment variable, or "NIFTI_GZ" if that is not set.

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")

    # Last argument is interpreted as a file type if valid
    if (nArguments() > 1 && isTRUE(toupper(Arguments[nArguments()]) %in% tractor.base:::.FileTypes$typeNames))
    {
        fileType <- toupper(Arguments[nArguments()])
        imageFileNames <- Arguments[-nArguments()]
    }
    else if (is.null(getOption("tractorFileType")))
    {
        # This should never happen with current .First.lib(), but keeping check here to be sure
        report(OL$Error, "No default file type is available - format must be given")
    }
    else
    {
        fileType <- getOption("tractorFileType")
        imageFileNames <- Arguments
    }
    
    for (fileName in imageFileNames)
    {
        image <- readImageFile(fileName)
        writeImageFile(image, fileType=fileType)
    }
    
    invisible(NULL)
}
