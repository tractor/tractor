#@args [session directory]

library(tractor.session)

runExperiment <- function ()
{
	session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
	
	algorithm <- getConfigVariable("Algorithm", "tournier", validValues=c("tournier", "tax", "dhollander"))
	order <- getConfigVariable("Order", NULL, "integer")
	
	mrtrixDir <- session$getDirectory("mrtrix")
	if (!file.exists(file.path(mrtrixDir, "wmresp.txt")))
		runWorkflow("dwi2response", session, Algorithm=algorithm, Order=order)
	runWorkflow("dwi2fod", session, Order=order)
}
