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
