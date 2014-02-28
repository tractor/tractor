#@args [graph file]
#@desc Convert a Graph object, stored in an ".Rdata" file, into a comma-separated list of numbers representing the corresponding association or adjacency matrix. The object may be specified as an argument (which takes priority), or through the GraphName option.

library(tractor.base)
library(tractor.graph)

runExperiment <- function ()
{
    graphName <- getConfigVariable("GraphName", NULL, "character")
    
    if (nArguments() > 0)
        fileName <- ensureFileSuffix(implode(Arguments[1]," "), "Rdata")
    else
        fileName <- ensureFileSuffix(graphName, "Rdata")
    
    graph <- deserialiseReferenceObject(fileName)
    fileName <- ensureFileSuffix(fileName, "csv", strip="Rdata")
    write.table(graph$getAssociationMatrix(), fileName, sep=",", row.names=FALSE, col.names=FALSE)
}
