#@desc Creates a version of the specified file that uses a small datatype to limit disk usage. If no output file is specified then the input file will be overwritten.
#@args image file, [output file]

runExperiment <- function ()
{
    requireArguments("image file")
    
    maxSize <- getConfigVariable("MaxBytesPerPixel", 2L)
    
    image <- readImageFile(Arguments[1])
    writeImageFile(image, Arguments[nArguments()], maxSize=maxSize)
}
