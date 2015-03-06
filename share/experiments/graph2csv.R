#@args [graph file]
#@desc Convert a Graph object, stored in an ".Rdata" file, into a comma-separated list of numbers representing the corresponding association or adjacency matrix. The diagonal runs top-left to bottom-right in this case. Vertex attributes are included as comments at the top of the file. The object may be specified as an argument (which takes priority), or through the GraphName option.

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
    connection <- file(fileName, "w")
    
    attribStrings <- sapply(graph$getVertexAttributes(), function(attrib) paste0(": ", implode(attrib,",")))
    if (length(attribStrings) > 0)
    {
        attribStrings <- paste0("# ", names(graph$getVertexAttributes()), attribStrings)
        writeLines(attribStrings, connection)
    }
    
    write.table(graph$getAssociationMatrix(), connection, sep=",", row.names=FALSE, col.names=FALSE)
    
    close(connection)
}
