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
        
        # First edge
        expect_equivalent(t_graph$getEdge(1), drop(igraph::ends(i_graph,1)))
        
        # Number of Vertices
        expect_equal(t_graph$nVertices(), igraph::gorder(i_graph))
    
        # Connected Vertices
        i_connectedVertices <- which(igraph::components(i_graph)$membership > 0)
        expect_equivalent(t_graph$getConnectedVertices(), i_connectedVertices)
        
        # Edge Density
        expect_equal(t_graph$getEdgeDensity(), igraph::graph.density(i_graph))
    })
    
    test_that("shortest paths match", {
        if (t_graph$isWeighted())
            igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
        
        # Shortest Path
        i_shortest_paths <- igraph::distances(i_graph)
        expect_equivalent(t_graph$getShortestPathMatrix(), i_shortest_paths)
        
        # Mean Shortest Path
        nonzero <- (i_shortest_paths > 0)
        i_mean_shortest <- sum(i_shortest_paths[nonzero]) / sum(nonzero)
        expect_equal(t_graph$getMeanShortestPath(), i_mean_shortest)
    })
    
    test_that("clustering coefficients and efficiencies match", {
        # igraph returns NaN when there are no triangles
        i_clustering <- igraph::transitivity(i_graph, "barrat")
        expect_equal(t_graph$getClusteringCoefficients(method="barrat"), ifelse(is.nan(i_clustering),0,i_clustering))
        
        if (t_graph$isWeighted())
            igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
        
        connected <- which(igraph::degree(i_graph) > 0)
        i_eff <- 1 / igraph::distances(i_graph, connected)
        i_global_eff <- mean(i_eff[upper.tri(i_eff) | lower.tri(i_eff)], na.rm=TRUE)
        expect_equivalent(graphEfficiency(t_graph,type="global"), i_global_eff)
    })
    
    test_that("betweenness centrality matches", {
        if (t_graph$isWeighted())
            igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
        
        t_bc <- structure(betweennessCentrality(t_graph$getAssociationMatrix()), dim=NULL)
        i_bc <- igraph::betweenness(i_graph)
        expect_equal(t_bc, 2*i_bc)
    })
    
    test_that("Laplacian matrices match", {
        expect_equivalent(t_graph$getLaplacianMatrix(), igraph::laplacian_matrix(i_graph,sparse=FALSE))
    })
}
