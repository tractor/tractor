#@args image file(s), R expression
#@desc Generate a new image by applying an R expression to one or more input images. (The expression should be quoted to prevent it from being interpreted by the shell.) The original images are represented in the expression using the symbols a, b, c, etc. The number of input images is usually guessed, but can be specified explicitly with the Inputs variable. If the Combine option is given then the function will be applied to each image in turn, and then the results combined. If the result is a single value or a simple vector then it will be printed, otherwise a new image is created and written to file. Metadata for the result image will be based on the first image specified.
#@example # Create a new image from the log of the voxelwise sum of two others
#@example tractor apply image1 image2 "log(a+b)"
#@example # Threshold and binarise two images, then add them together
#@example tractor apply image1 image2 "ifelse(a>=1,1,0)" Combine:sum

runExperiment <- function ()
{
    requireArguments("image file(s)", "R expression")
    
    nInputs <- getConfigVariable("Inputs", NULL, "integer")
    combine <- getConfigVariable("Combine", NULL, "character", validValues=c("mean","sum","prod","min","max"))
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
        
        report(OL$Verbose, "Guessing that there are #{nInputs} input images")
    }
    
    if (nInputs < 1)
        report(OL$Error, "At least one input image must be specified")
    
    images <- lapply(Arguments[1:nInputs], readImageFile)
    expression <- implode(Arguments[-(1:nInputs)], sep=" ")
    funString <- paste("function(", implode(letters[1:nInputs],sep=","), ") { ", expression, " }", sep="")
    report(OL$Debug, "Function string is `", funString, "`")
    fun <- eval(parse(text=funString))
    
    if (is.null(combine))
        result <- do.call(fun, lapply(images, function(x) x$getData()))
    else
    {
        combine <- match.fun(combine)
        data <- array(NA_real_, c(dim(images[[1]]),nInputs))
        args <- alist(i=,j=,k=,t=,u=,v=,w=)[1:images[[1]]$getDimensionality()]
        for (i in seq_len(nInputs))
        {
            values <- do.call(fun, list(images[[i]]$getData()))
            data <- do.call("[<-", c(list(data),args,list(i,values)))
        }
        result <- apply(data, 1:images[[1]]$getDimensionality(), combine, na.rm=TRUE)
    }
    
    if (length(result) == 1 || (!is.array(result) && !(length(result) %in% cumprod(dim(images[[1]])))))
        cat(paste0(implode(result,sep="\n"), "\n"))
    else
    {
        resultImage <- asMriImage(result, images[[1]])
        writeImageFile(resultImage, resultName)
    }
}
