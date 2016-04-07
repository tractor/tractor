#@args [graph file]
#@desc Convert a Graph object, stored in an ".Rdata" file, into a comma-separated list of numbers representing the corresponding association or adjacency matrix. The diagonal runs top-left to bottom-right in this case. Vertex attributes are included as comments at the top of the file. The object may be specified as an argument (which takes priority), or through the GraphName option.

library(tractor.base)
library(tractor.graph)

runExperiment <- function ()
{
    graphName <- getConfigVariable("GraphName", NULL, "character")
    
    if (nArguments() > 0)
        fileName <- ensureFileSuffix(implode(Arguments[1]," "), NULL, strip=c("Rdata","csv"))
    else
        fileName <- ensureFileSuffix(graphName, NULL, strip=c("Rdata","csv"))
    
    graph <- readGraphFile(fileName, "binary")
    writeGraphFile(graph, fileName, "csv")
}
