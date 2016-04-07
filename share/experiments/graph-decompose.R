#@desc Decompose a graph into parts by factoring or partitioning it. Available algorithms for this include "principal networks" (Clayden et al., PLoS ONE, 2013) and modularity maximisation (Newman, PNAS, 2006). An edge weight threshold (ignoring sign) can be applied to the results if required. Eigenvalue and loading thresholds apply to the principal networks approach only.

library(tractor.graph)

runExperiment <- function ()
{
    graphName <- getConfigVariable("GraphName", NULL, "character", errorIfMissing=TRUE)
    method <- getConfigVariable("Method", "principal-networks", validValues=c("principal-networks","modularity"))
    edgeWeightThreshold <- getConfigVariable("EdgeWeightThreshold", 0)
    eigenvalueThreshold <- getConfigVariable("EigenvalueThreshold", NULL, "numeric")
    loadingThreshold <- getConfigVariable("LoadingThreshold", 0.1)
    dropTrivial <- getConfigVariable("DropTrivial", TRUE)
    
    graph <- readGraphFile(graphName)
    outputFileName <- paste(graphName, "decomposed", sep="_")
    
    if (method == "principal-networks")
    {
        decomposition <- principalNetworks(graph, eigenvalueThreshold=eigenvalueThreshold, loadingThreshold=loadingThreshold, edgeWeightThreshold=edgeWeightThreshold)
        printLoadings(decomposition$loadings)
        serialiseReferenceObject(decomposition$componentGraphs, outputFileName)
    }
    else
    {
        if (graph$isWeighted())
            graph <- thresholdEdges(graph, edgeWeightThreshold, ignoreSign=TRUE)
        
        partition <- partitionGraph(graph, method=method)
        subgraphs <- lapply(partition, function(x) inducedSubgraph(graph,x))
        serialiseReferenceObject(subgraphs, outputFileName)
    }
}
