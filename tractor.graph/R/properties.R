edgeDensity <- function (graph, selfConnections = FALSE)
{
    graph <- asGraph(graph)
    
    edges <- graph$getEdges()
    nVertices <- graph$nVertices()
    nEdges <- graph$nEdges() - ifelse(selfConnections, 0, sum(edges[,1]==edges[,2]))
    nPossibleEdges <- ifelse(graph$isDirected(), nVertices^2, nVertices*(nVertices+1)/2) - ifelse(selfConnections, 0, nVertices)
    
    return (nEdges / nPossibleEdges)
}

vertexDegree <- function (graph, type = c("all","in","out"))
{
    graph <- asGraph(graph)
    adjacencyMatrix <- graph$getAdjacencyMatrix()
    
    type <- match.arg(type)
    if (!graph$isDirected())
        return (rowSums(adjacencyMatrix) + diag(adjacencyMatrix))
    else if (type == "all")
        return (rowSums(adjacencyMatrix) + colSums(adjacencyMatrix))
    else if (type == "out")
        return (rowSums(adjacencyMatrix))
    else
        return (colSums(adjacencyMatrix))
}

vertexStrength <- function (graph, type = c("all","in","out"))
{
    graph <- asGraph(graph)
    associationMatrix <- graph$getAssociationMatrix()
    
    type <- match.arg(type)
    if (!graph$isDirected())
        return (rowSums(associationMatrix) + diag(associationMatrix))
    else if (type == "all")
        return (rowSums(associationMatrix) + colSums(associationMatrix))
    else if (type == "out")
        return (rowSums(associationMatrix))
    else
        return (colSums(associationMatrix))
}

connectedVertices <- function (graph, type = c("all","in","out"))
{
    return (which(vertexDegree(graph,type) > 0))
}

neighbourhoods <- function (graph, vertices = NULL, type = c("all","in","out"), simplify = TRUE)
{
    graph <- asGraph(graph)
    
    type <- match.arg(type)
    if (is.null(vertices))
        vertices <- graph$getVertices()
    
    neighbourhoods <- .Call("neighbourhoods", .graphPointer(graph), vertices, type, PACKAGE="tractor.graph")
    
    if (simplify && length(neighbourhoods) == 1)
        return (neighbourhoods[[1]])
    else
        return (neighbourhoods)
}

shortestPaths <- function (graph)
{
    .Call("shortestPaths", .graphPointer(graph), PACKAGE="tractor.graph")
}

meanShortestPath <- function (graph, ignoreInfinite = TRUE)
{
    graph <- asGraph(graph)
    
    shortestPathMatrix <- shortestPaths(graph)
    if (ignoreInfinite)
        shortestPathMatrix[is.infinite(shortestPathMatrix)] <- NA
    
    return (mean(shortestPathMatrix[!diag(graph$nVertices())], na.rm=TRUE))
}

laplacianMatrix <- function (graph)
{
    graph <- asGraph(graph)
    
    if (graph$isDirected())
        report(OL$Error, "Laplacian matrix calculation for directed graphs is not yet implemented")
    
    associationMatrix <- graph$getAssociationMatrix()
    degreeMatrix <- diag(colSums(associationMatrix))
    return (degreeMatrix - associationMatrix)
}

clusteringCoefficients <- function (graph, method = c("onnela","barrat"))
{
    method <- match.arg(method)
    .Call("clusteringCoefficients", .graphPointer(graph), method, PACKAGE="tractor.graph")
}

graphEfficiency <- function (graph, type = c("global","local"))
{
    if (!is(graph, "Graph"))
        report(OL$Error, "Specified graph is not a valid Graph object")
    
    type <- match.arg(type)
    
    v <- connectedVertices(graph)
    if (length(v) < 2)
    {
        if (type == "global")
            return (0)
        else
            return (rep(0,length(v)))
    }
    
    if (type == "global")
    {
        sp <- shortestPaths(graph)
        ge <- mean(1/sp[upper.tri(sp) | lower.tri(sp)])
        return (ge)
    }
    else
    {
        n <- neighbourhoods(graph)
        le <- sapply(n, function (cn) {
            if (length(cn) < 2)
                return (0)
            else
            {
                subgraph <- inducedSubgraph(graph, cn)
                sp <- shortestPaths(subgraph)
                return (mean(1/sp[upper.tri(sp) | lower.tri(sp)]))
            }
        })
        return (le)
    }
}

betweennessCentrality <- function (weight_matrix)
{
    # Convert weight matrix in connection-length matrix
    # diag(weight_matrix) <- 0
    if (asGraph(weight_matrix)$isDirected())
        stop("Betweenness Centrality is currently only implemented for undirected graphs")
    
    nonzero_weights <- which(weight_matrix>0)
    weight_matrix[nonzero_weights] <- 1/(weight_matrix[nonzero_weights])
    nNodes <- dim(weight_matrix)[1]
    
    # Initialize betweenness centrality to 0 for all nodes
    betw_centr <- array(0, nNodes)
    
    # Initialization: for each node curNode
    for (curNode in 1:nNodes)
    {
        # 1. Mark curNode as unvisited by setting the distance between startNode and curNode to infiity
        dist_matrix <- array(Inf, nNodes)
        dist_matrix[curNode] <- 0
        
        # 2. Setlist of curNode's predecessors on a shortest Path to the empty list
        pred_matrix <- matrix(rep(F, (nNodes*nNodes)), nNodes, nNodes)
        
        # 3. Set the list of all shortest paths from startNode to curNode to the empty list 
        num_paths <- array(0, nNodes)
        num_paths[curNode] <- 1
        
        G1 <- weight_matrix
        S <- rep(T, nNodes)
        Queue <- rep(0, nNodes)
        q <- nNodes
        V <- curNode
        while (TRUE)
        {
            S[V] <- 0
            G1[,V] <- 0
            for (v in V)
            {
                W <- which(as.logical(G1[v,]))
                Queue[q] <- v
                q <- q-1
                for (w in W)
                {
                    DuW <- dist_matrix[v] + G1[v,w]
                    if (DuW < dist_matrix[w])
                    {
                        dist_matrix[w] <- DuW
                        num_paths[w] <- num_paths[v]
                        pred_matrix[w,] <- 0
                        pred_matrix[w,v] <- 1
                    }
                    else if (DuW == dist_matrix[w])
                    {
                        num_paths[w] <- num_paths[w] + num_paths[v]
                        pred_matrix[w,v] <- 1
                    }
                }
            }
            D <- dist_matrix[as.logical(S)]
            if (length(D) == 0) break 
                minD <- min(D)
            if (is.infinite(minD))
            {
                Queue[1:q] <- which(is.infinite(dist_matrix)) 
                break
            }
            V <- which(dist_matrix == minD) 
        }
        DP <- array(0,nNodes)
        for (w in Queue[1:(nNodes-1)])
        {
            betw_centr[w] <- betw_centr[w] + DP[w]
            for (v in which(pred_matrix[w,]>0))
                DP[v] <- DP[v]+(1+DP[w]) * num_paths[v]/num_paths[w]
        }
    }
    return (betw_centr)
}
