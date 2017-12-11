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
        expect_equivalent(t_graph$getShortestPathMatrix(), igraph::distances(i_graph))
        
        # Mean Shortest Path
        expect_equal(t_graph$getMeanShortestPath(), igraph::mean_distance(i_graph))
    })
    
    test_that("clustering coefficients and efficiencies match", {
        expect_equal(t_graph$getClusteringCoefficients(), igraph::transitivity(i_graph,"barrat"))
        
        i_eff <- 1 / igraph::distances(i_graph)
        i_eff[!is.finite(i_eff)] <- 0
        i_global_eff <- mean(i_eff, na.rm=TRUE) 
        expect_equivalent(graphEfficiency(t_graph,type="global"), i_global_eff)
    })
    
    test_that("Laplacian matrices match", {
        expect_equivalent(t_graph$getLaplacianMatrix(), igraph::laplacian_matrix(i_graph,sparse=FALSE))
    })
    
    test_that("betweenness centrality matches", {
        if (t_graph$isWeighted())
            igraph::E(i_graph)$weight <- 1 / igraph::E(i_graph)$weight
        
        t_bc <- betweennessCentrality(t_graph$getAssociationMatrix())
        i_bc <- igraph::betweenness(i_graph)
        expect_equal(t_bc, 2*i_bc)
    })
}
