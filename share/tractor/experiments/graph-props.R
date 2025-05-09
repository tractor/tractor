#@desc Print out various graph-theoretical properties of a graph object. Any edge weight threshold specified is applied before the calculations. If IgnoreSign:true is given then then edges with weights greater than the threshold or less than its negative will be retained; this will be done by default if the threshold is positive. Note that further properties of the graph, such as the attributes which are defined for it, may be seen using the "peek" script.
#@args graph file
#@group Graph and network analysis
#@nohistory TRUE

library(tractor.base)
library(tractor.graph)

runExperiment <- function ()
{
    requireArguments("graph file")
    
    edgeWeightThreshold <- getConfigVariable("EdgeWeightThreshold", 0, "numeric")
    ignoreSign <- getConfigVariable("IgnoreSign", NULL, "logical")
    binarise <- getConfigVariable("Binarise", TRUE)
    disconnectedVertices <- getConfigVariable("DisconnectedVertices", FALSE)
    normalise <- getConfigVariable("Normalise", FALSE)
    wccMethod <- getConfigVariable("WeightedClusteringCoefficient", "onnela", validValues=c("onnela","barrat"))
    
    graph <- readGraphFile(implode(Arguments, " "))
    
    if (is.null(ignoreSign))
        ignoreSign <- (edgeWeightThreshold >= 0)
    if (edgeWeightThreshold != 0 || !ignoreSign || binarise)
        graph <- thresholdEdges(graph, edgeWeightThreshold, ignoreSign=ignoreSign, binarise=binarise)
    
    nVertices <- graph$nVertices()
    nConnectedVertices <- length(graph$getConnectedVertices())
    if (!disconnectedVertices)
        graph <- inducedSubgraph(graph)
    
    if (normalise)
        graph$map(fx(x / max(x,na.rm=TRUE)))
    
    nSelfConnections <- sum(graph$getEdges()[,1] == graph$getEdges()[,2])
    meanAbsEdgeWeight <- mean(abs(graph$getEdgeWeights()), na.rm=TRUE)
    edgeWeightRange <- range(graph$getEdgeWeights(), na.rm=TRUE)
    meanShortestPath <- meanShortestPath(graph, ignoreInfinite=TRUE)
    globalEfficiency <- graphEfficiency(graph, type="global")
    localEfficiency <- mean(graphEfficiency(graph, type="local"), na.rm=TRUE)
    meanClusteringCoefficient <- mean(clusteringCoefficients(graph, method=wccMethod), na.rm=TRUE)
    
    values <- c(es("#{nVertices} (#{nConnectedVertices} connected)"),
                es("#{graph$nEdges()} (#{nSelfConnections} self-connections)"),
                es("#{edgeDensity(graph)*100}%",round=2),
                es("#{meanAbsEdgeWeight} (range: #{edgeWeightRange[1]} to #{edgeWeightRange[2]})",signif=3),
                es("#{meanShortestPath} #{ifelse(graph$isWeighted(),'(inverse weight)','steps')}",signif=3),
                signif(c(globalEfficiency, localEfficiency, meanClusteringCoefficient),3))
    labels <- c("Number of vertices", "Number of edges", "Edge density", "Mean absolute edge weight", "Mean finite shortest path", "Global efficiency", "Mean local efficiency", "Mean clustering coefficient")
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    
    if (binarise)
        printLabelledValues(labels[-4], values[-4])
    else
        printLabelledValues(labels, values)
}
