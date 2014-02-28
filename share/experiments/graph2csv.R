#@args file name
#@desc Convert a Graph object stored in an ".Rdata" file into a comma-separated list of numbers representing the corresponding association or adjacency matrix.

library(tractor.base)
library(tractor.graph)

runExperiment <- function ()
{
    requireArguments("file name")
    
    fileName <- ensureFileSuffix(Arguments[1], "Rdata")
    graph <- deserialiseReferenceObject(fileName)
    fileName <- ensureFileSuffix(fileName, "csv", strip="Rdata")
    write.table(graph$getAssociationMatrix(), fileName, sep=",", row.names=FALSE, col.names=FALSE)
}
