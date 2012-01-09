Graph <- setRefClass("Graph", contains="SerialisableObject", fields=list(vertexCount="integer",vertexNames="character",vertexLocations="matrix",locationUnit="character",edges="matrix",edgeNames="character",edgeWeights="numeric",directed="logical"), methods=list(
    initialize = function (vertexCount = 0, vertexNames = NULL, vertexLocations = matrix(NA,0,0), locationUnit = "", edges = matrix(NA,0,0), edgeNames = character(0), edgeWeights = rep(NA,nrow(edges)), directed = FALSE)
    {
        return (initFields(vertexCount=as.integer(vertexCount), vertexNames=as.character(vertexNames), vertexLocations=vertexLocations, locationUnit=locationUnit, edges=edges, edgeNames=edgeNames, edgeWeights=edgeWeights, directed=directed))
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
    
    nVertices = function () { return (vertexCount) }
))

newGraphFromTable <- function (table, method = c("correlation","covariance"), threshold = NULL, ignoreSign = FALSE, allVertexNames = NULL)
{
    method <- match.arg(method)
    
    if (method == "correlation")
        connectionMatrix <- cor(table)
    else if (method == "covariance")
        connectionMatrix <- cov(table)
    
    return (newGraphFromConnectionMatrix(connectionMatrix, threshold=threshold, ignoreSign=ignoreSign, directed=FALSE, allVertexNames=allVertexNames))
}

newGraphFromConnectionMatrix <- function (connectionMatrix, threshold = NULL, ignoreSign = FALSE, directed = FALSE, allVertexNames = NULL)
{
    if (!is.null(threshold))
    {
        if (ignoreSign)
            connectionMatrix[abs(connectionMatrix) < threshold] <- NA
        else
            connectionMatrix[connectionMatrix < threshold] <- NA
    }
    
    if (!directed)
        connectionMatrix[lower.tri(connectionMatrix,diag=TRUE)] <- NA
    
    if (is.null(allVertexNames))
        allVertexNames <- union(rownames(connectionMatrix), colnames(connectionMatrix))
    rowVertexLocs <- match(rownames(connectionMatrix), allVertexNames)
    colVertexLocs <- match(colnames(connectionMatrix), allVertexNames)
    
    edges <- which(!is.na(connectionMatrix), arr.ind=TRUE)
    edgeWeights <- connectionMatrix[edges]
    edges[,1] <- rowVertexLocs[edges[,1]]
    edges[,2] <- colVertexLocs[edges[,2]]
    dimnames(edges) <- NULL
    
    return (Graph$new(vertexCount=length(allVertexNames), vertexNames=allVertexNames, edges=edges, edgeWeights=edgeWeights, directed=FALSE))   
}
