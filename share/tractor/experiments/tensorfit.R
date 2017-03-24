#@args [session directory]
#@desc Fit diffusion tensors for the specified session directory (default "."). The Method option can be "ls" for ordinary least-squares fitting, or "iwls" for an iterative weighted least-squares approach, which takes longer but is more robust to heteroskedasticity in the log-transformed data. Method:fsl is essentially equivalent to Method:ls, except that FSL's "dtifit" program is used to perform the fit. If FSL is installed, this is usually the fastest option.

library(tractor.session)

runExperiment <- function ()
{
    session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
    
    method <- getConfigVariable("Method", "ls", validValues=c("ls","iwls","fsl"))
    
    if (method != "iwls" && session$getDiffusionScheme()$nShells() > 1)
    {
        report(OL$Info, "Using IWLS, since diffusion acquisition scheme has multiple shells")
        method <- "iwls"
    }
    
    if (method == "fsl")
        runDtifitWithSession(session)
    else
        createDiffusionTensorImagesForSession(session, method)
    
    invisible(NULL)
}
