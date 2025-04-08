#@desc Decompose a graph into parts by factoring or partitioning it. Available algorithms for this include "principal networks" (Clayden et al., PLoS ONE, 2013) and modularity maximisation (Newman, PNAS, 2006). An edge weight threshold (ignoring sign) can be applied to the results if required. Eigenvalue and loading thresholds apply to the principal networks approach only. If a reference partition is specified, it will be applied to the graph, and the Method option will be ignored.
#@args graph file
#@group Graph and network analysis

library(tractor.graph)

runExperiment <- function ()
{
    requireArguments("graph file")
    
    method <- getConfigVariable("Method", "principal-networks", validValues=c("principal-networks","modularity","connected"))
    refPartitionFile <- getConfigVariable("ReferencePartition", NULL, "character")
    edgeWeightThreshold <- getConfigVariable("EdgeWeightThreshold", 0)
    eigenvalueThreshold <- getConfigVariable("EigenvalueThreshold", NULL, "numeric")
    loadingThreshold <- getConfigVariable("LoadingThreshold", 0.1)
    dropTrivial <- getConfigVariable("DropTrivial", TRUE)
    
    graphName <- implode(Arguments, " ")
    graph <- readGraphFile(graphName)
    fileStem <- ensureFileSuffix(basename(graphName), NULL, strip=c("Rdata","csv"))
    
    if (!is.null(refPartitionFile))
    {
        report(OL$Info, "Applying the reference partition")
        refPartition <- deserialiseReferenceObject(refPartitionFile)
        partition <- applyPartition(refPartition, graph)
        subgraphs <- lapply(partition$getCommunities(), fx(inducedSubgraph(graph,x)))
    }
    else if (method == "principal-networks")
    {
        report(OL$Info, "Decomposing into principal networks")
        partition <- principalNetworks(graph)$graph
        partition$mask(vertices=fx(abs(x) >= loadingThreshold), communities=fx(x >= eigenvalueThreshold))
        printLoadings(partition$getVertexWeights())
        subgraphs <- lapply(seq_len(partition$nCommunities()), fi(thresholdEdges(partition[[i]], edgeWeightThreshold, ignoreSign=TRUE)))
    }
    else
    {
        if (graph$isWeighted())
            graph <- thresholdEdges(graph, edgeWeightThreshold, ignoreSign=TRUE)
        
        report(OL$Info, "Partitioning based on #{ifelse(method=='modularity','modularity','connectedness')}")
        partition <- partitionGraph(graph, method=method, dropTrivial=dropTrivial)
        subgraphs <- lapply(partition$getCommunities(), fx(inducedSubgraph(graph,x)))
    }
    
    partition$serialise(paste(fileStem, "partitioned", sep="_"))
    serialiseReferenceObject(subgraphs, paste(fileStem,"decomposed",sep="_"))
}
