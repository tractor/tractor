#@args transform directory, [source image file]
#@desc Visualise the deformation field corresponding to the specified transformation. A slice axis and location in voxels (using the R convention, indexing from 1) should be given in TARGET space, although the visualisation is shown in source space, projected into the plane that is most closely correspondent. The overlay shows the projection of the target space in source space, with each point coloured according to the local Jacobian determinant: red indicates that the transformation produces a local expansion, and blue a local contraction. If the source image is not provided, its location will be determined from the metadata stored with the transformation if possible. If Reverse:true is given, the inverse transformation will be visualised if it is available. Nonlinear transformations will take priority unless PreferAffine:true is given.
#@group Registration
#@interactive TRUE
#@nohistory TRUE

library(tractor.reg)

runExperiment <- function ()
{
    preferAffine <- getConfigVariable("PreferAffine", FALSE)
    reverse <- getConfigVariable("Reverse", FALSE)
    x <- getConfigVariable("X", NA, "numeric", errorIfInvalid=TRUE)
    y <- getConfigVariable("Y", NA, "numeric", errorIfInvalid=TRUE)
    z <- getConfigVariable("Z", NA, "numeric", errorIfInvalid=TRUE)
    
    requireArguments("transform directory")
    
    if (nArguments() > 1)
        sourceImage <- readImageFile(implode(Arguments[-1]," "))
    else
        sourceImage <- NULL
    
    reg <- readRegistration(Arguments[1])
    
    # FIXME: there is currently no plot() method for Registration objects
    plot(reg, xLoc=x, yLoc=y, zLoc=z, sourceImage=sourceImage, preferAffine=preferAffine, reverse=reverse)
    
    if (ask("Copy figure to pdf file? [yn]", valid=c("y","n")) == "y")
        dev.print(pdf, file=ensureFileSuffix(transformName,"pdf",strip="Rdata"))
    
    invisible(NULL)
}
