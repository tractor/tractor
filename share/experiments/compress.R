#@desc Creates a version of the specified file that uses a small datatype to limit disk usage.
#@args image file

runExperiment <- function ()
{
    requireArguments("image file")
    
    maxSize <- getConfigVariable("MaxBytesPerPixel", 2L)
    
    image <- readImageFile(implode(Arguments, sep=" "))
    writeImageFile(image, paste(basename(image$getSource()), "compressed", sep="_"), maxSize=maxSize)
}
