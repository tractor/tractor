#@args [source image file]
#@desc Visualise the deformation field corresponding to the specified transformation. A slice axis and location in voxels (using the R convention, indexing from 1) should be given in TARGET space, although the visualisation is shown in source space, projected into the plane that is most closely correspondent. The overlay shows the projection of the target space in source space, with each point coloured according to the local Jacobian determinant: red indicates that the transformation produces a local expansion, and blue a local contraction. If the source image is not provided, its location will be determined from the metadata stored with the transformation if possible. If Reverse:true is given, the inverse transformation will be visualised if it is available. Nonlinear transformations will take priority unless PreferAffine:true is given.
#@interactive TRUE
#@nohistory TRUE

library(tractor.reg)

runExperiment <- function ()
{
    transformName <- getConfigVariable("TransformationName", NULL, "character")
    preferAffine <- getConfigVariable("PreferAffine", FALSE)
    reverse <- getConfigVariable("Reverse", FALSE)
    x <- getConfigVariable("X", NA, "numeric", errorIfInvalid=TRUE)
    y <- getConfigVariable("Y", NA, "numeric", errorIfInvalid=TRUE)
    z <- getConfigVariable("Z", NA, "numeric", errorIfInvalid=TRUE)
    
    if (nArguments() > 0)
        sourceImage <- readImageFile(Arguments[1])
    else
        sourceImage <- NULL
    
    transform <- attachTransformation(transformName)
    if (!is(transform, "Transformation"))
        report(OL$Error, "The specified file does not contain a valid Transformation object")
    
    plot(transform, xLoc=x, yLoc=y, zLoc=z, sourceImage=sourceImage, preferAffine=preferAffine, reverse=reverse)
    
    ans <- ask("Copy figure to pdf file? [yn]")
    if (tolower(ans) == "y")
        dev.print(pdf, file=ensureFileSuffix(transformName,"pdf",strip="Rdata"))
    
    invisible(NULL)
}
