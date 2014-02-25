#@args image file(s), R expression
#@desc Generate a new image by applying an R expression to one or more input images. The original images are represented in the expression using the symbols a, b, c, etc. The number of input images is usually guessed, but can be specified explicitly with the Inputs variable. If the result is a single value then it will be printed, otherwise a new image is created and written to file. Metadata for the result image will be based on the first image specified.
#@example # Create a new image from the log of the voxelwise sum of two others
#@example tractor apply image1 image2 "log(a+b)"

runExperiment <- function ()
{
    nInputs <- getConfigVariable("Inputs", NULL, "integer")
    resultName <- getConfigVariable("ResultName", "result")
    
    if (is.null(nInputs))
    {
        nInputs <- 0
        for (i in seq_len(nArguments()))
        {
            if (imageFileExists(Arguments[i]))
                nInputs <- nInputs + 1
            else
                break
        }
        
        report(OL$Debug, "Guessing that there are #{nInputs} input images")
    }
    
    if (nInputs < 1)
        report(OL$Error, "At least one input image must be specified")
    
    images <- lapply(Arguments[1:nInputs], readImageFile)
    expression <- implode(Arguments[-(1:nInputs)], sep=" ")
    funString <- paste("function(", implode(letters[1:nInputs],sep=","), ") { ", expression, " }", sep="")
    report(OL$Debug, "Function string is `", funString, "`")
    fun <- eval(parse(text=funString))
    
    result <- do.call(fun, lapply(images, function(x) x$getData()))
    
    if (length(result) == 1)
        cat(paste(result, "\n", sep=""))
    else
    {
        resultImage <- newMriImageWithData(result, images[[1]])
        writeImageFile(resultImage, resultName)
    }
}
