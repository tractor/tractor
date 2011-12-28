Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(nVertices="integer",vertexNames="character",vertexLocations="matrix",locationUnit="character",edges="matrix",edgeNames="character",edgeWeights="numeric",directed="logical"), methods=list(
    getEdge = function (i)
    {
        if (i < 0 || i > .self$nEdges())
            return (NA)
        else
            return (edges[i,])
    },
    
    getEdges = function () { return (edges) },
    
    getEdgeNames = function () { return (edgeNames) },
    
    getEdgeWeights = function () { return (edgeWeights) },
    
    getVertexLocations = function () { return (vertexLocations) },
    
    getVertexLocationUnit = function () { return (locationUnit) },
    
    getVertexNames = function () { return (vertexNames) },
    
    isDirected = function () { return (directed) },
    
    nEdges = function () { return (nrow(edges)) },
    
    nVertices = function () { return (nVertices) }
))
