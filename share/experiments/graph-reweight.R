#@args R expression
#@desc Reweight a graph object using edge and/or vertex attributes already stored with it. There are a lot of possible weighting schemes so this is done by specifying a general R expression in terms of the attribute names. (These can be seen using "peek".) Since there are two vertices associated with each edge (one at each end), the vertex attributes must be merged into a single value before they can be used; the VertexAttributes option allows this to be done in various ways.
#@example # Weight edges by the number of streamlines connecting the two vertex, divided by mean voxel count
#@example tractor graph-reweight "nStreamlines/voxelCount" VertexAttributes:mean GraphName:graph

library(tractor.graph)

runExperiment <- function ()
{
    graphName <- getConfigVariable("GraphName", NULL, "character", errorIfMissing=TRUE)
    vertexAttributeFunction <- getConfigVariable("VertexAttributes", "ignore", validValues=c("ignore","sum","mean","max","min"))
    
    graph <- deserialiseReferenceObject(graphName)
    attributes <- graph$getEdgeAttributes()
    
    if (vertexAttributeFunction != "ignore")
    {
        vertexAttributes <- lapply(names(graph$getVertexAttributes()), function(attribName) {
            values <- graph$getVertexAttributes(attribName)[as.vector(graph$getEdges())]
            if (!is.numeric(values))
                return (NULL)
            else
            {
                values <- matrix(values, ncol=2)
                return (apply(values, 1, vertexAttributeFunction, na.rm=TRUE))
            }
        })
        names(vertexAttributes) <- names(graph$getVertexAttributes())
        
        # For now we don't expect this, so warn instead of resolving the clash
        if (any(names(vertexAttributes) %in% names(attributes)))
            report(OL$Warning, "Some vertex attribute names clash with edge attribute names")
        
        attributes <- c(attributes, vertexAttributes)
    }
    
    expression <- implode(Arguments, " ")
    weights <- eval(parse(text=expression), envir=attributes, enclos=baseenv())
    
    if (!is.numeric(weights))
        report(OL$Error, "The result of the specified expression is not numeric")
    else if (length(weights) != graph$nEdges())
        report(OL$Error, "Weights have length #{length(weights)}, but there are #{graph$nEdges()} edges in the graph")
    else
    {
        graph$setEdgeWeights(weights)
        graph$serialise(graphName)
    }
}
