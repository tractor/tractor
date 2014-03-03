#@desc Print out various graph-theoretical properties of a graph object. The object may be specified as an argument (which takes priority), or through the GraphName option. This script requires the "igraph" R package. Note that further properties of the graph, such as the attributes which are defined for it, may be seen using the "peek" script.
#@args [graph file]
#@nohistory TRUE

library(tractor.base)
library(igraph)
library(tractor.graph)

runExperiment <- function ()
{
    graphName <- getConfigVariable("GraphName", NULL, "character")
    edgeWeightThreshold <- getConfigVariable("EdgeWeightThreshold", 0, "numeric")
    binarise <- getConfigVariable("Binarise", TRUE)
    disconnectedVertices <- getConfigVariable("DisconnectedVertices", FALSE)
    
    if (nArguments() > 0)
        graph <- deserialiseReferenceObject(implode(Arguments, " "))
    else
        graph <- deserialiseReferenceObject(graphName)
    
    if (!is(graph, "Graph"))
        report(OL$Error, "The specified file does not contain a valid graph object")
    
    if (edgeWeightThreshold > 0)
        graph <- thresholdEdges(graph, edgeWeightThreshold, ignoreSign=TRUE, binarise=binarise)
    else if (edgeWeightThreshold < 0 || binarise)
        graph <- thresholdEdges(graph, edgeWeightThreshold, ignoreSign=FALSE, binarise=binarise)
    
    meanAbsEdgeWeight <- mean(abs(graph$getEdgeWeights()), na.rm=TRUE)
    edgeWeightRange <- range(graph$getEdgeWeights(), na.rm=TRUE)
    meanShortestPath <- meanShortestPath(graph, ignoreInfinite=!disconnectedVertices)
    globalEfficiency <- graphEfficiency(graph, type="global")
    localEfficiency <- mean(graphEfficiency(graph, type="local"), na.rm=TRUE)
    meanClusteringCoefficient <- mean(clusteringCoefficients(graph), na.rm=TRUE)
    values <- c(s("#{graph$nVertices()} (#{length(graph$getConnectedVertices())} connected)"), graph$nEdges(), s("#{graph$getEdgeDensity(disconnectedVertices=disconnectedVertices)*100}%",round=2), s("#{meanAbsEdgeWeight} (range: #{edgeWeightRange[1]} to #{edgeWeightRange[2]})",signif=3), s("#{meanShortestPath} #{ifelse(graph$isWeighted(),'(inverse weight)','steps')}",signif=3), signif(c(globalEfficiency, localEfficiency, meanClusteringCoefficient),3))
    labels <- c("Number of vertices", "Number of edges", "Edge density", "Mean absolute edge weight", "Mean shortest path", "Global efficiency", "Mean local efficiency", "Mean clustering coefficient")
    
    if (binarise)
        printLabelledValues(labels[-4], values[-4])
    else
        printLabelledValues(labels, values)
}
