#@desc Run the specified workflow, a bash script typically interfacing a third-party tool to TractoR, on a session directory (default "."). Any subsequent unnamed arguments are passed to the environment of the workflow (in which case the session directory must be specified), as are all named configuration variables. Any of these may therefore be used in its script.
#@args workflow name, [session directory], [arguments]

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("workflow name")
    sessionPath <- ifelse(nArguments() < 2, ".", Arguments[2])
    do.call(runWorkflow, c(list(Arguments[1],sessionPath), ConfigVariables, list(.args=Arguments[-(1:2)])))
    ConfigVariables <<- NULL
}
