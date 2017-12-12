# library(tractor.graph)
# library(igraph)
# library(brainwaver)
context("Weighted Graph")

createSymMatrix <- function(vector, dim)
{
    m <- matrix(rep(0,dim*dim), nrow=dim)
    m[lower.tri(m)] <- vector
    m <- t(m)
    m[lower.tri(m)] <- vector
    return(m)
}

# Graph 0: Undirected, weighted
m <- createSymMatrix(seq(from=1,to=choose(10,2),by=1), 10)
m[2,] = 0
m[,2] = 0
t_graph <- asGraph.matrix(m)
i_graph <- graph_from_adjacency_matrix(m, mode="undirected", weighted=TRUE) 
t_association_matrix <- t_graph$getAssociationMatrix()
i_association_matrix <- as.matrix(as_adjacency_matrix(i_graph, attr="weight"))

test_that("Graphs are created sucessfully", {
    # Associationmatrix
    t_association_matrix <- t_graph$getAssociationMatrix()
    i_association_matrix <- as.matrix(as_adjacency_matrix(i_graph, attr="weight"))
    expect_equivalent(t_association_matrix,i_association_matrix)
    
    # GRAPH ATTRIBUTES
    expect_false(t_graph$isDirected())
    
    # Weighted
    expect_true(t_graph$isWeighted())
})

test_that("Setter functions work",{
    volume1 <- runif(10, 0,5)
    voxelCount1 <- round(runif(10,0,20))
    names1 <- c("a","b","c","d","e","f","g","h","i","j")
    t_graph$setVertexAttributes(voxelCount1, volume1, names1)
    expect_identical(t_graph$getVertexAttributes(), list(voxelCount1, volume1, names1))
    t_graph <- asGraph.matrix(m)
    
    # Attribute vectors too long
    volume2 <- runif(20, 0,5)
    voxelCount2 <- round(runif(20,0,20))
    names2 <- c("a","b","c","d","e","f","g","h","i","j","k", "l", "m", "n")
    t_graph$setVertexAttributes(voxelCount2, volume2, names2)
    expect_identical(t_graph$getVertexAttributes(), list(voxelCount2[1:10], volume2[1:10], names2[1:10]))
    t_graph <- asGraph.matrix(m)
    
    # Same value for all edges 
    t_graph$setVertexAttributes(1,2,"a")
    expect_identical(t_graph$getVertexAttributes(), list(rep(1,10), rep(2,10), rep("a",10)))
    t_graph <- asGraph.matrix(m)
    
    # Set Edge Weights
    n = sum(t_graph$getEdgeWeights()>0)
    new_weights <- runif(n,0,2)
    t_graph$setEdgeWeights(new_weights)
    expect_equal(t_graph$getEdgeWeights(), new_weights)
    
    # Set Association Matrix
    m_new <- createSymMatrix(c(runif(45,0,2)), 10)
    m_new[3,] = 0
    m_new[,3] = 0 
    t_graph$setAssociationMatrix(m_new)
    expect_equivalent(t_graph$getAssociationMatrix(), m_new)
    
    # Vertex Locations
    locations <- matrix(runif(30,0,180),10,3)
    t_graph$setVertexLocations(locations, "mm", "some/directory")
    expect_equivalent(t_graph$getVertexLocations(), locations)
})

test_that("Edge and vertex tests", {
    # Number of Edges
    expect_equal(ecount(i_graph), t_graph$nEdges())
    
    # Edge(s)
    t_new_ass_m <- t_graph$getAssociationMatrix()
    t_new_ass_m[lower.tri(t_new_ass_m, diag = TRUE)] <- 0 
    expect_equivalent(t_graph$getEdge(1), which(t_new_ass_m>0, arr.ind = TRUE)[1,]) 
    expect_equivalent(t_graph$getEdges(), which(t_new_ass_m>0, arr.ind = TRUE))
    
    # Number of Vertices
    expect_equal(vcount(i_graph), t_graph$nVertices())
    
    # Connected Vertices
    t_connected_v <- t_graph$getConnectedVertices()
    # i_connected_v <- seq(1:t_graph$nVertices())[(components(i_graph)$membership > 0)]
    i_connected_v <- seq(1, vcount(i_graph))[degree(i_graph)>0]
    expect_equivalent(t_connected_v, i_connected_v)
})

test_that("Edge Normalisation", {
    # NORMALISE EDGE WEIGHTS
    t_graph$normaliseEdgeWeights()
    t_norm_weights <- t_graph$getEdgeWeights()
    # t_norm_control <- ass_m[upper.tri(ass_m)]/max(ass_m[upper.tri(ass_m)])
    i_norm_weights <- E(i_graph)$weight / max(E(i_graph)$weight)
    tmp <- m[upper.tri(m)]
    tmp <- cbind(t_norm_weights,tmp[tmp>0])
    t_norm_weights <- tmp[order(tmp[,1])]
    expect_identical(t_norm_weights, i_norm_weights)
})

test_that("Weights detected correctly", {
    # WEIGHTS
    t_graph <- asGraph.matrix(m)
    t_weights <- t_graph$getEdgeWeights()
    t_control_weights <- t_association_matrix[upper.tri(t_association_matrix)]
    t_control_weights <- t_control_weights[t_control_weights>0]
    expect_true(all(t_weights==t_control_weights))
})

test_that("Edge Density", {
    # EDGE DENSITY
    t_density <- t_graph$getEdgeDensity()
    i_density <- graph.density(i_graph)
    expect_equal(t_density, i_density)
})

test_that("Shortest Path Measures", {
    # PATHS
    # Shortest Path
    weight_matrix <- t_graph$getAssociationMatrix()
    nonzero_weights <- which(weight_matrix>0)
    weight_matrix[nonzero_weights] <- 1/(weight_matrix[nonzero_weights])
    i_length_graph <- graph_from_adjacency_matrix(weight_matrix, mode="undirected", weighted=TRUE)
    i_shortest_path <- distances(i_length_graph)
    t_shortest_path <- t_graph$getShortestPathMatrix()
    expect_equivalent(t_shortest_path, i_shortest_path)

    # Mean Shortest Path
    t_mean_shortest <- t_graph$getMeanShortestPath()
    # i_mean_shortest <- mean_distance(i_graph, directed = FALSE, unconnected = FALSE)
    i_shortest_path[which(i_shortest_path==Inf)] = 0
    i_mean_shortest <- sum(i_shortest_path[i_shortest_path>0])/sum(i_shortest_path>0)
    expect_equal(t_mean_shortest, i_mean_shortest)
})

test_that("Clustering Coefficient", {
    # CLUSTERING COEFFICIENT
    t_clustering <- t_graph$getClusteringCoefficients()
    i_clustering <- transitivity(i_graph, "barrat")
    expect_equal(t_clustering, i_clustering)
})

test_that("Local and global efficiency", {
    # EFFICIENCY
    t_adjacency_matrix <- ifelse(t_association_matrix>0,1,0)
    bw <- global.efficiency(t_adjacency_matrix,t_association_matrix)$eff
    t_global_eff <- graphEfficiency(t_graph, type="global")
    t_local_eff <- graphEfficiency(t_graph, type="local")
})

test_that("Local and global efficiency", {
    # EFFICIENCY
    # Brainwaver implementation
    #t_association_matrix <- t_graph$getAssociationMatrix()
    #i_association_matrix <- as.matrix(as_adjacency_matrix(i_graph, attr=NULL))
    #t_adjacency_matrix <- ifelse(t_association_matrix>0,1,0)
    #bw <- global.efficiency(t_adjacency_matrix,t_association_matrix)$eff
    
    # Test Code
    i_eff <- 1/(shortest.paths(i_graph))
    i_eff[!is.finite(i_eff)] <- 0
    i_global_eff<-mean(i_eff,na.rm=TRUE) 
    
    t_global_eff <- graphEfficiency(t_graph, type="global")
    t_local_eff <- graphEfficiency(t_graph, type="local")
    expect_equivalent(t_global_eff, i_global_eff)
})

test_that("Laplacian Matrix", {
    t_graph <- asGraph.matrix(m)
    i_graph <- graph_from_adjacency_matrix(m, mode="undirected", weighted=TRUE) 
    t_association_matrix <- t_graph$getAssociationMatrix()
    
    # LAPLACIAN MATRIX
    t_laplacian <- t_graph$getLaplacianMatrix()
    i_laplacian <- as.matrix(laplacian_matrix(i_graph))
    expect_equivalent(t_laplacian, i_laplacian)
}) 

test_that("Graph Conversion", {
    t_graph <- asGraph.matrix(m)
    i_graph <- graph_from_adjacency_matrix(m, mode="undirected", weighted=TRUE) 
    
    # CONVERSION
    # From RGraph to iGraph an reversed
    i_graph_converted <- as(t_graph, "igraph")
    ass_mat_converted <- as.matrix(as_adjacency_matrix(i_graph_converted, attr="weight"))
    expect_equivalent(ass_mat_converted, i_association_matrix)
    
    # From Graph to Matrix
    ass_mat_retrieved <- as(t_graph, "matrix")
    expect_equivalent(ass_mat_retrieved, t_association_matrix)
    
    # From Matrix to Graph
    t_graph_converted <- as(ass_mat_retrieved, "Graph")
    expect_equivalent(t_graph_converted, t_graph)
    
    ass_mat_retrieved1 <- as.matrix.Graph(t_graph)
    expect_identical(ass_mat_retrieved1, t_association_matrix)
})

test_that("Betweenness centrality works fine", {
    t_bc <- betweennessCentrality(t_graph$getAssociationMatrix())
    copy_m <- m
    diag(copy_m) <- 0
    nonzero_weights <- which(copy_m>0)
    copy_m[nonzero_weights] <- 1/(copy_m[nonzero_weights])
    i_con_length <- graph_from_adjacency_matrix(copy_m, mode="undirected", weighted=TRUE) 
    i_bc <- as.array(estimate_betweenness(i_con_length, cutoff = -1, directed = TRUE, weights = NULL))
    expect_equal(t_bc, 2*i_bc)
})