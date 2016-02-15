#@desc Create a subgraph by extracting only a subset of the nodes of the specified graph, and their interconnections. Nodes may be specified by name or parcellation index, separated by spaces or commas.
#@args session directory, regions to retain

library(tractor.reg)
library(tractor.session)
library(tractor.graph)

runExperiment <- function ()
{
    graphName <- getConfigVariable("GraphName", NULL, "character", errorIfMissing=TRUE)
    subgraphName <- getConfigVariable("SubgraphName", NULL, "character", errorIfMissing=TRUE)
    
    graph <- deserialiseReferenceObject(graphName)
    session <- attachMriSession(Arguments[1])
    parcellation <- session$getParcellation()
    regionNames <- matchRegions(splitAndConvertString(Arguments[-1],",",fixed=TRUE), parcellation, labels=TRUE)
    
    nodeNames <- graph$getVertexAttributes("name")
    if (is.null(nodeNames))
        report(OL$Error, "The specified graph does not contain vertex names")
    
    indices <- match(regionNames, nodeNames)
    if (any(is.na(indices)))
        report(OL$Warning, "Regions ", implode(regionNames[is.na(indices)],",",finalSep=" and "), " are not present in the graph")
    
    subgraph <- inducedSubgraph(graph, na.omit(indices))
    subgraph$serialise(subgraphName)
}
