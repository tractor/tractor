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
        
        rownames(vertexWeights) <- .self$getVertexAttributes("name")
        
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
            if (all(x == 0))
                return (NA)
            
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
    
    mask = function (vertices = NULL, communities = NULL)
    {
        if (is.function(vertices))
            vertices <- vertices(.self$vertexWeights)
        if (!is.null(vertices))
            .self$vertexWeights <- .self$vertexWeights * as.logical(vertices)
        
        if (is.function(communities))
            communities <- communities(.self$communityWeights)
        if (!is.null(communities))
            .self$communityWeights <- .self$communityWeights * as.logical(communities)
        
        zeroWeightCommunities <- which(.self$communityWeights == 0)
        if (length(zeroWeightCommunities) > 0)
        {
            .self$vertexWeights <- .self$vertexWeights[,-zeroWeightCommunities]
            .self$communityWeights <- .self$communityWeights[-zeroWeightCommunities]
        }
        
        invisible(.self)
    },
    
    nCommunities = function () { return (ncol(vertexWeights)) },
    
    summarise = function ()
    {
        values <- c(callSuper(),
                    "Number of communities"=.self$nCommunities(),
                    "Community coverage"=es("#{(1-sum(is.na(.self$getVertexMemberships()))/.self$nVertices())*100}%", round=2))
        return (values)
    }
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

setMethod("[[", signature(x="PartitionedGraph",i="ANY"), function (x,i) {
    index <- as.integer(i)
    if (length(index) != 1)
        report(OL$Error, "Index must be a single integer")
    return (inducedSubgraph(x, vertices=x$getCommunities(index)))
})

modularityMatrix <- function (graph)
{
    graph <- asGraph(graph)
    
    associationMatrix <- graph$getAssociationMatrix()
    diag(associationMatrix) <- diag(associationMatrix) * 2
    totalWeight <- sum(associationMatrix, na.rm=TRUE)
    modularityMatrix <- associationMatrix - (vertexStrength(graph,"in") %o% vertexStrength(graph,"out")) / totalWeight
    
    return (modularityMatrix)
}

modularity <- function (graph, ...)
{
    graph <- asPartitionedGraph(graph, ...)
    
    memberships <- graph$getVertexMemberships()
    matchingMembershipMatrix <- outer(memberships, memberships, fxy(ifelse(!is.na(x) & !is.na(y) & x==y, 1, 0)))
    modularity <- sum(modularityMatrix(graph) * matchingMembershipMatrix) / (2 * sum(graph$getEdgeWeights(), na.rm=TRUE))
    
    return (modularity)
}

partitionGraph <- function (graph, method = "modularity")
{
    graph <- asGraph(graph, strict=TRUE)
    method <- match.arg(method)
    
    if (method == "modularity")
    {
        # Following Newman (PNAS, 2006), generalised to weighted graphs
        totalWeight <- 2 * sum(graph$getEdgeWeights(), na.rm=TRUE)
        modularityMatrix <- modularityMatrix(graph)
        
        findPartition <- function (indices)
        {
            modularitySubmatrix <- modularityMatrix[indices,indices,drop=FALSE]
            diag(modularitySubmatrix) <- diag(modularitySubmatrix) - rowSums(modularitySubmatrix)
            eigensystem <- eigen(modularitySubmatrix)
            
            groupMembership <- ifelse(eigensystem$vectors[,1] >= 0, 1, -1)
            firstGroupIndices <- indices[which(groupMembership < 0)]
            secondGroupIndices <- indices[which(groupMembership >= 0)]
            modularityIncrease <- (matrix(groupMembership,nrow=1) %*% modularitySubmatrix %*% matrix(groupMembership,ncol=1)) / (2 * totalWeight)
            
            if (length(firstGroupIndices) > 0 && length(secondGroupIndices) > 0 && modularityIncrease > 0)
                return (c(findPartition(firstGroupIndices), findPartition(secondGroupIndices)))
            else
                return (list(indices))
        }
        
        communities <- findPartition(connectedVertices(graph))
        report(OL$Info, "Graph has been partitioned into #{length(communities)} parts, containing #{implode(sapply(communities,length),sep=', ',finalSep=' and ')} vertices")
        
        return (asPartitionedGraph(graph, communities=communities))
    }
}

applyPartition <- function (partition, graph)
{
    partition <- asPartitionedGraph(partition)
    graph <- asGraph(graph, strict=TRUE)
    
    if (partition$nVertices() != graph$nVertices())
        report(OL$Error, "Number of vertices must match the partitioned graph")
    
    # Community weights are the eigenvalues for principal networks, or the sum of vertex strengths within communities
    vertexWeights <- partition$getVertexWeights()
    communityWeights <- t(vertexWeights) %*% graph$getAssociationMatrix() %*% vertexWeights
    return (asPartitionedGraph(graph, vertexWeights=vertexWeights, communityWeights=communityWeights))
}
