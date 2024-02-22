testMetricAgreement <- function (t_graph, i_graph = as(t_graph,"igraph"))
{
    info <- paste0("Matrix was matrix(c(", paste(signif(as.matrix(t_graph),4),collapse=","), "), ", t_graph$nVertices(), ", ", t_graph$nVertices(), ")")
    
    # Basic properties
    expect_equal(t_graph$isWeighted(), igraph::is_weighted(i_graph), info=info)
    expect_equal(t_graph$isDirected(), igraph::is_directed(i_graph), info=info)
    
    if (t_graph$isWeighted())
        i_associationMatrix <- igraph::as_adjacency_matrix(i_graph, attr="weight", sparse=FALSE)
    else
        i_associationMatrix <- igraph::as_adjacency_matrix(i_graph, attr=NULL, sparse=FALSE)
    
    expect_equivalent(t_graph$getAssociationMatrix(), i_associationMatrix, info=info)
    
    # Number of edges
    expect_equal(t_graph$nEdges(), igraph::gsize(i_graph), info=info)
    
    # First edge
    expect_equivalent(t_graph$getEdges(1), igraph::ends(i_graph,1), info=info)
    
    # Number of vertices
    expect_equal(t_graph$nVertices(), igraph::gorder(i_graph), info=info)

    # Vertex degree and strength
    expect_equivalent(vertexDegree(t_graph), igraph::degree(i_graph), info=info)
    expect_equivalent(vertexStrength(t_graph), igraph::strength(i_graph), info=info)
    
    # Edge density
    t_density <- edgeDensity(t_graph, selfConnections=t_graph$isSelfConnected())
    i_density <- igraph::edge_density(i_graph, loops=t_graph$isSelfConnected())
    expect_equal(t_density, i_density, info=info)
    
    # Some metrics are not supported by igraph for directed graphs
    if (!t_graph$isDirected())
    {
        # Clustering coefficients
        # igraph returns NaN when there are no triangles
        i_clustering <- igraph::transitivity(i_graph, "barrat", isolates="zero")
        expect_equal(clusteringCoefficients(t_graph,method="barrat"), i_clustering, info=info)
        
        # Laplacian matrix
        expect_equivalent(laplacianMatrix(t_graph), igraph::laplacian_matrix(i_graph,sparse=FALSE), info=info)
    }
    
    # Convert weights to costs
    # NB: this is needed only for quantities tested below, so timing of the
    # inversion is significant
    if (t_graph$isWeighted())
        igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
    
    # Shortest path
    i_shortest_paths <- igraph::distances(i_graph, mode="out")
    expect_equivalent(shortestPaths(t_graph), i_shortest_paths, info=info)
    
    # Mean shortest path
    nonzero <- is.finite(i_shortest_paths) & (i_shortest_paths != 0)
    i_mean_shortest <- sum(i_shortest_paths[nonzero]) / sum(nonzero)
    expect_equal(meanShortestPath(t_graph), i_mean_shortest, info=info)
    
    # Global efficiency
    i_eff <- 1 / igraph::distances(i_graph, mode="out")
    i_global_eff <- mean(i_eff[upper.tri(i_eff) | lower.tri(i_eff)], na.rm=TRUE)
    expect_equivalent(graphEfficiency(t_graph,type="global"), i_global_eff, info=info)
    
    # Some metrics are not supported by igraph for directed graphs
    if (!t_graph$isDirected())
    {
        # Betweenness centrality
        t_bc <- structure(betweennessCentrality(t_graph$getAssociationMatrix()), dim=NULL)
        i_bc <- igraph::betweenness(i_graph)
        expect_equal(t_bc, 2*i_bc, info=info)
    }
}

if (!requireNamespace("igraph", quietly=TRUE))
    exit_file("The \"igraph\" package is not available")

# Random binary graph
testMetricAgreement(randomGraph(10, M=20))

# Random weighted graph
testMetricAgreement(randomGraph(10, weights=runif(20)))

# Disconnected vertex
graph <- randomGraph(10, weights=runif(20))
graph[,1] <- 0
graph[1,] <- 0
testMetricAgreement(graph)

# Disjoint union of five-vertex subgraphs
graph <- randomGraph(10, weights=runif(20))
graph[1:5,6:10] <- 0
graph[6:10,1:5] <- 0
testMetricAgreement(graph)

# Negative weight
weights <- runif(20)
# All positive costs (inverse weights) must be at least as high in magnitude as this, to avoid negative loops
weights[1] <- -1
graph <- randomGraph(10, weights=weights, directed=TRUE)
testMetricAgreement(graph)

# Random directed graph
testMetricAgreement(randomGraph(10, M=60, directed=TRUE))

# Random self-connected graph
# NB: igraph 2.0.x currently seems to drop self-connections from the adjacency matrix
graph <- randomGraph(10, M=30, selfConnections=TRUE)
if (packageVersion("igraph") < "2.0")
    testMetricAgreement(graph)
