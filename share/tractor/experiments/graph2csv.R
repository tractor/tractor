#@args graph file
#@desc Convert a Graph object, stored in an ".Rdata" file, into a comma-separated list of numbers representing the corresponding association or adjacency matrix. The diagonal runs top-left to bottom-right in this case. Vertex attributes are included as comments at the top of the file.
#@group Graph and network analysis

library(tractor.base)
library(tractor.graph)

runExperiment <- function ()
{
    requireArguments("graph file")
    
    fileName <- ensureFileSuffix(implode(Arguments," "), NULL, strip=c("Rdata","csv"))
    graph <- readGraphFile(fileName, "binary")
    writeGraphFile(graph, fileName, "csv")
}
