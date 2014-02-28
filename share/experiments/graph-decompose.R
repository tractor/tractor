runExperiment <- function ()
{
    # eigenvalueThreshold = NULL, loadingThreshold = 0.1, iterations = 0, confidence = 0.95, edgeWeightThreshold = 0.2, dropTrivial = TRUE
    graphName <- getConfigVariable("GraphName", NULL, "character", errorIfMissing=TRUE)
    method <- getConfigVariable("Method", "principal-networks", validValues=c("principal-networks","modularity"))
    eigenvalueThreshold <- getConfigVariable("EigenvalueThreshold", NULL, "numeric")
    loadingThreshold <- getConfigVariable("LoadingThreshold", 0.1)
    edgeWeightThreshold <- getConfigVariable("EdgeWeightThreshold", 0.2)
    dropTrivial <- getConfigVariable("DropTrivial", TRUE)
    
    graph <- deserialiseReferenceObject(graphName)
    
    if (method == "principal-networks")
    {
        decomposition <- principalNetworks(graph, eigenvalueThreshold=eigenvalueThreshold, loadingThreshold=loadingThreshold, edgeWeightThreshold=edgeWeightThreshold, dropTrivial=dropTrivial)
    }
    else if (method == "modularity")
    {
        
    }
}
