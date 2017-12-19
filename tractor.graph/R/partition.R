GraphPartition <- setRefClass("GraphPartition", contains="SerialisableObject", fields=list(method="character",communityCount="integer",vertexWeights="matrix",communityWeights="numeric"), methods=list(
    initialize = function (method = "none", communityCount = 0, vertexWeights = emptyMatrix(), communityWeights = numeric(0), ...)
    {
        initFields(method=method, communityCount=communityCount, vertexWeights=vertexWeights, communityWeights=communityWeights)
    },
    
    getCommunitySizes = function () { return (colSums(vertexWeights != 0)) },
    
    getCommunityWeights = function () { return (communityWeights) },
    
    getMethod = function () { return (method) },
    
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
    
    nCommunities = function () { return (communityCount) },
    
    nVertices = function () { return (nrow(vertexWeights)) }
))

asGraphPartition <- function (x, ...)
{
    UseMethod("asGraphPartition")
}

asGraphPartition.list <- function (x, method = "unknown", ...)
{
    nVertices <- do.call(max, c(x, list(na.rm=TRUE)))
    communityCount <- length(x)
    vertexWeights <- do.call(cbind, lapply(seq_along(x), function(i) {
        column <- rep(0L, nVertices)
        column[x[[i]]] <- 1L
        return (column)
    }))
    
    GraphPartition$new(method=method, communityCount=communityCount, vertexWeights=vertexWeights, communityWeights=numeric(0))
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

modularity <- function (graph, partition)
{
    graph <- asGraph(graph)
    partition <- as(partition, "GraphPartition")
    
    if (graph$nVertices() != partition$nVertices())
        report(OL$Error, "Number of vertices is not the same in the graph and partition objects")
    
    memberships <- partition$getVertexMemberships()
    modularity <- sum(modularityMatrix(graph) * outer(memberships, memberships, function(x,y) ifelse(x==y,1,0))) / (2 * graph$nEdges())
    
    return (modularity)
}

partitionGraph <- function (graph, method = "modularity")
{
    if (!is(graph, "Graph"))
        report(OL$Error, "The specified graph is not a valid Graph object")
    
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
        
        partitionList <- findPartition(graph$getConnectedVertices())
        report(OL$Info, "Graph has been partitioned into #{length(partition)} parts, containing #{implode(sapply(partition,length),sep=', ',finalSep=' and ')} vertices")
        
        return (asGraphPartition(partitionList, method=method))
    }
}
