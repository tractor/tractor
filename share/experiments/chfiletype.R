#@args image file, [format]
#@desc Reads and rewrites the specified Analyze/NIfTI image file using the specified
#@desc file format. The format can be "ANALYZE" (for Analyze format), "NIFTI" (for
#@desc single-file NIfTI), "NIFTI_PAIR" (NIfTI header/image pair), or any of these
#@desc with "_GZ" appended for the equivalent formats compressed with gzip(1). If
#@desc the format is not specified, it will be taken from the TRACTOR_FILETYPE
#@desc environment variable. An Analyze/NIfTI data type code can also be specified
#@desc with the TypeCode option, perhaps for compatibility with other software.

library(tractor.base)

runExperiment <- function ()
{
    requireArguments("image file")

    datatype <- getWithDefault("TypeCode", NULL, "integer")
    if (!is.null(datatype))
        datatype <- getDataTypeByNiftiCode(datatype)
    
    image <- newMriImageFromFile(Arguments[1])
    
    if (nArguments() > 1)
        writeMriImageToFile(image, fileType=toupper(Arguments[2]), datatype=datatype)
    else if (is.null(getOption("tractorFileType")))
        output(OL$Error, "No default file type is available - format must be given")
    else
        writeMriImageToFile(image, fileType=getOption("tractorFileType"), datatype=datatype)
    
    invisible(NULL)
}
