#@args [session directory]
#@desc Fit diffusion tensors for the specified session directory (default "."). The Method option can be "ls" for ordinary least-squares fitting, or "iwls" for an iterative weighted least-squares approach, which takes longer but is more robust to heteroskedasticity in the log-transformed data. Method:fsl is essentially equivalent to Method:ls, except that FSL's "dtifit" program is used to perform the fit. If FSL is installed, this is usually the fastest option.
#@group Diffusion processing

library(tractor.session)

runExperiment <- function ()
{
    session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
    
    method <- getConfigVariable("Method", "ls", validValues=c("ls","iwls","fsl","fsl-wls"))
    
    if (method == "ls" && session$getDiffusionScheme()$nShells() > 1)
    {
        report(OL$Info, "Using IWLS, since diffusion acquisition scheme has multiple shells")
        method <- "iwls"
    }
    else if (method == "fsl" && session$getDiffusionScheme()$nShells() > 1)
    {
        report(OL$Info, "Using FSL-WLS, since diffusion acquisition scheme has multiple shells")
        method <- "fsl-wls"
    }
    
    if (method %in% c("fsl","fsl-wls"))
        runDtifitWithSession(session, weightedLeastSquares=(method=="fsl-wls"))
    else
        createDiffusionTensorImagesForSession(session, method)
    
    invisible(NULL)
}
