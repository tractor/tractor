#@desc Run the specified workflow on a session directory (default "."). Named configuration variables are passed to the environment of the workflow, and may be used in its 
#@args workflow name, [session directory]

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("workflow name")
    sessionPath <- ifelse(nArguments() < 2, ".", Arguments[2])
    do.call(runWorkflow, c(list(Arguments[1],sessionPath), ConfigVariables))
    ConfigVariables <<- NULL
}
