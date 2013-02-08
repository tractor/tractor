#@desc Create a graph based on cortical thickness data from a group of subjects. FreeSurfer must have been run first (see "freesurf").

library(tractor.session)
library(tractor.graph)

runExperiment <- function ()
{
    sessionList <- getConfigVariable("SessionList", NULL, errorIfMissing=TRUE)
    
    sessionList <- lapply(sessionList, newSessionFromDirectory)
    data <- createCorticalThicknessTableForSessions(sessionList)
    graph <- newGraphFromTable(data, method="correlation")
    
    graph$serialise("ctgraph.Rdata")
    
    invisible(NULL)
}
