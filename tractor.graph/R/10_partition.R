PartitionedGraph <- setRefClass("PartitionedGraph", contains="Graph", fields=list(vertexWeights="matrix",communityWeights="numeric"), methods=list(
    initialize = function (graph = NULL, vertexWeights = emptyMatrix(), communityWeights = numeric(0), ...)
    {
        if (!is.null(graph))
            import(graph, "Graph")
        else
            callSuper(...)
        
        if (nrow(vertexWeights) != vertexCount)
            report(OL$Error, "Vertex weight matrix should have #{vertexCount} rows (one per vertex)")
        if (length(communityWeights) > 0 && length(communityWeights) != ncol(vertexWeights))
            report(OL$Error, "Inputs don't agree on the number of communities")
        
        initFields(vertexWeights=vertexWeights, communityWeights=communityWeights)
    },
    
    getCommunities = function (i = NULL)
    {
        if (is.null(i))
            lapply(seq_len(ncol(vertexWeights)), function(j) which(vertexWeights[,j] != 0))
        else if (length(i) == 1)
            which(vertexWeights[,i] != 0)
        else
            lapply(i, function(j) which(vertexWeights[,j] != 0))
    },
    
    getCommunitySizes = function () { return (colSums(vertexWeights != 0)) },
    
    getCommunityWeights = function ()
    {
        if (length(communityWeights) == 0)
            return (rep(1, .self$nCommunities()))
        else
            return (communityWeights)
    },
    
    getVertexMemberships = function ()
    {
        memberships <- apply(vertexWeights, 1, function(x) {
            maxWeight <- which(x == max(x,na.rm=TRUE))
            if (length(maxWeight) > 1)
            {
                flag(OL$Warning, "Maximum vertex weight is not unique")
                sample(maxWeight, 1)
            }
            else
                return (maxWeight)
        })
        
        return (memberships)
    },
    
    getVertexWeights = function () { return (vertexWeights) },
    
    nCommunities = function () { return (ncol(vertexWeights)) }
))

asPartitionedGraph <- function (x, ...)
{
    UseMethod("asPartitionedGraph")
}

asPartitionedGraph.PartitionedGraph <- function (x, ...)
{
    return (x)
}

asPartitionedGraph.Graph <- function (x, vertexWeights, communities, communityWeights = numeric(0), ...)
{
    if (missing(vertexWeights) && missing(communities))
        vertexWeights <- matrix(1, nrow=x$nVertices(), ncol=1L)
    else if (missing(vertexWeights) && !missing(communities))
    {
        vertexWeights <- do.call(cbind, lapply(seq_along(communities), function(i) {
            column <- rep(0L, x$nVertices())
            column[communities[[i]]] <- 1L
            return (column)
        }))
    }
    
    PartitionedGraph$new(x, vertexWeights=vertexWeights, communityWeights=communityWeights)
}

asPartitionedGraph.matrix <- function (x, ...)
{
    asPartitionedGraph.Graph(asGraph.matrix(x), ...)
}

asGraph.PartitionedGraph <- function (x, strict = FALSE, ...)
{
    if (strict)
        return (x$export("Graph"))
    else
        return (x)
}

modularityMatrix <- function (graph)
{
    graph <- asGraph(graph)
    
    adjacencyMatrix <- graph$getAdjacencyMatrix()
    diag(adjacencyMatrix) <- diag(adjacencyMatrix) * 2
    degree <- graph$getVertexDegree()
    nEdges <- graph$nEdges()
    modularityMatrix <- adjacencyMatrix - (degree %o% degree) / (2 * nEdges)
    
    return (modularityMatrix)
}

modularity <- function (graph, ...)
{
    graph <- asPartitionedGraph(graph, ...)
    
    memberships <- graph$getVertexMemberships()
    modularity <- sum(modularityMatrix(graph) * outer(memberships, memberships, function(x,y) ifelse(x==y,1,0))) / (2 * graph$nEdges())
    
    return (modularity)
}

partitionGraph <- function (graph, method = "modularity")
{
    graph <- asGraph(graph, strict=TRUE)
    method <- match.arg(method)
    
    if (method == "modularity")
    {
        # Following Newman (PNAS, 2006)
        nEdges <- graph$nEdges()
        modularityMatrix <- modularityMatrix(graph)
        
        findPartition <- function (indices)
        {
            modularitySubmatrix <- modularityMatrix[indices,indices,drop=FALSE]
            diag(modularitySubmatrix) <- diag(modularitySubmatrix) - rowSums(modularitySubmatrix)
            eigensystem <- eigen(modularitySubmatrix)
            
            groupMembership <- sign(sign(eigensystem$vectors[,1]) + 0.5)
            firstGroupIndices <- indices[which(groupMembership < 0)]
            secondGroupIndices <- indices[which(groupMembership >= 0)]
            modularityIncrease <- (matrix(groupMembership,nrow=1) %*% modularitySubmatrix %*% matrix(groupMembership,ncol=1)) / (4 * nEdges)
            
            if (length(firstGroupIndices) > 0 && length(secondGroupIndices) > 0 && modularityIncrease > 0)
                return (c(findPartition(firstGroupIndices), findPartition(secondGroupIndices)))
            else
                return (list(indices))
        }
        
        communities <- findPartition(graph$getConnectedVertices())
        report(OL$Info, "Graph has been partitioned into #{length(communities)} parts, containing #{implode(sapply(communities,length),sep=', ',finalSep=' and ')} vertices")
        
        return (asPartitionedGraph(graph, communities=communities))
    }
}
