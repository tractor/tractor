testMetricAgreement <- function (t_graph, i_graph = as(t_graph,"igraph"))
{
    test_that("TractoR and igraph objects match", {
        expect_equal(t_graph$isWeighted(), igraph::is_weighted(i_graph))
        expect_equal(t_graph$isDirected(), igraph::is_directed(i_graph))
        
        if (t_graph$isWeighted())
            i_associationMatrix <- igraph::as_adjacency_matrix(i_graph, attr="weight", sparse=FALSE)
        else
            i_associationMatrix <- igraph::as_adjacency_matrix(i_graph, attr=NULL, sparse=FALSE)
    
        expect_equivalent(t_graph$getAssociationMatrix(), i_associationMatrix)
    })
    
    test_that("core properties match", {
        # Number of Edges
        expect_equal(t_graph$nEdges(), igraph::gsize(i_graph))
        
        # First Edge
        expect_equivalent(t_graph$getEdges(1), drop(igraph::ends(i_graph,1)))
        
        # Number of Vertices
        expect_equal(t_graph$nVertices(), igraph::gorder(i_graph))
    
        # Vertex Degree and Stength
        expect_equivalent(vertexDegree(t_graph), igraph::degree(i_graph))
        expect_equivalent(vertexStrength(t_graph), igraph::strength(i_graph))
        
        # Edge Density
        t_density <- edgeDensity(t_graph, selfConnections=t_graph$isSelfConnected())
        i_density <- igraph::edge_density(i_graph, loops=t_graph$isSelfConnected())
        expect_equal(t_density, i_density)
    })
    
    test_that("shortest paths match", {
        if (t_graph$isWeighted())
            igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
        
        # Shortest Path
        i_shortest_paths <- igraph::distances(i_graph, mode="out")
        expect_equivalent(shortestPaths(t_graph), i_shortest_paths)
        
        # Mean Shortest Path
        nonzero <- is.finite(i_shortest_paths) & (i_shortest_paths != 0)
        i_mean_shortest <- sum(i_shortest_paths[nonzero]) / sum(nonzero)
        expect_equal(meanShortestPath(t_graph), i_mean_shortest)
    })
    
    test_that("clustering coefficients match", {
        skip_if(t_graph$isDirected(), "Directed clustering coefficients are not supported by igraph")
        
        # igraph returns NaN when there are no triangles
        i_clustering <- igraph::transitivity(i_graph, "barrat", isolates="zero")
        expect_equal(clusteringCoefficients(t_graph,method="barrat"), i_clustering)
    })
    
    test_that("global efficiency matches", {
        if (t_graph$isWeighted())
            igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
        
        connected <- which(igraph::degree(i_graph) > 0)
        i_eff <- 1 / igraph::distances(i_graph, connected, mode="out")
        i_global_eff <- mean(i_eff[upper.tri(i_eff) | lower.tri(i_eff)], na.rm=TRUE)
        expect_equivalent(graphEfficiency(t_graph,type="global"), i_global_eff)
    })
    
    test_that("betweenness centrality matches", {
        skip_if(t_graph$isDirected(), "Betweenness centrality is not supported for directed graphs")
        
        if (t_graph$isWeighted())
            igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
        
        t_bc <- structure(betweennessCentrality(t_graph$getAssociationMatrix()), dim=NULL)
        i_bc <- igraph::betweenness(i_graph)
        expect_equal(t_bc, 2*i_bc)
    })
    
    test_that("Laplacian matrices match", {
        skip_if(t_graph$isDirected(), "Laplacian matrix calculation is not supported for directed graphs")
        expect_equivalent(laplacianMatrix(t_graph), igraph::laplacian_matrix(i_graph,sparse=FALSE))
    })
}
