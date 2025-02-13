#@desc Run constrained spherical deconvolution (CSD) for the specified session using MRtrix, which must be installed and accessible for this to work. The default session is the current working directory. CSD works by assuming that the observed signal is a weighted combination of one or more copies of a fixed "response function" with different orientations; by estimating the response function(s) and then "deconvolving" the signal, the putative distribution of fibre directions can be recovered. These outputs are represented using the real spherical harmonic basis functions up to the specified maximum (even) order. There are several sub-algorithms available, which are detailed in the MRtrix documentation for the "dwi2response" binary. By default this script will select "dhollander" if the data has more than one nonzero b-value, producing response functions for multiple tissue types; otherwise it will choose "tournier".
#@args [session directory]

library(tractor.session)

runExperiment <- function ()
{
	session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
	
	algorithm <- getConfigVariable("Algorithm", "auto", validValues=c("auto", "tournier", "tax", "dhollander"))
	order <- getConfigVariable("Order", NULL, "integer")
	force <- getConfigVariable("Force", FALSE)
	
	if (algorithm == "auto")
	{
		nShells <- session$getDiffusionScheme()$nShells()
		algorithm <- ifelse(nShells > 1, "dhollander", "tournier")
		report(OL$Info, "Choosing \"#{algorithm}\" CSD algorithm for #{nShells}-shell data")
	}
	
	runWorkflow("dwi2response", session, Algorithm=algorithm, Order=order, Force=as.integer(force))
	runWorkflow("dwi2fod", session, Order=order, Force=as.integer(force))
}
