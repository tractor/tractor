Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(nVertices="integer",vertexNames="character",vertexLocations="matrix",locationUnit="character",edges="matrix",edgeNames="character",edgeWeights="numeric",directed="logical"), methods=list(
    initialize = function (nVertices = 0, vertexNames = NULL, vertexLocations = matrix(NA,0,0), locationUnit = "", edges = matrix(NA,0,0), edgeNames = character(0), edgeWeights = rep(NA,nrow(edges)), directed = FALSE)
    {
        return (initFields(nVertices=as.integer(nVertices), vertexNames=as.character(vertexNames), vertexLocations=vertexLocations, locationUnit=locationUnit, edges=edges, edgeNames=edgeNames, edgeWeights=edgeWeights, directed=directed))
    },
        
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
