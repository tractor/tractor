context("Subnetworks")

reportr::setOutputLevel(Warning)

test_that("partitioning works as expected", {
    graph <- readGraphFile("graph")
    graph$binarise()
    
    membershipMatches <- function (a, b) {
        all(sapply(unique(a), function(x) allEqual(b[a==x])))
    }
    
    t_partition <- partitionGraph(graph)
    expect_equal(t_partition$getMethod(), "modularity")
    expect_equal(t_partition$nCommunities(), 3L)
    expect_true(all(t_partition$getVertexWeights() %in% 0:1))
    i_partition <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_true(membershipMatches(t_partition$getVertexMemberships(), i_partition$membership))
    expect_equal(modularity(graph,t_partition), igraph::modularity(i_partition))
    
    graph <- randomGraph(10, M=20)
    t_partition <- partitionGraph(graph)
    i_partition <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_true(membershipMatches(t_partition$getVertexMemberships(), i_partition$membership))
    expect_equal(modularity(graph,t_partition), igraph::modularity(i_partition))
})

test_that("principal network decomposition works", {
    graph <- readGraphFile("graph")
    pn <- principalNetworks(graph)
    ref <- readRDS("p_network.rds")
    
    expect_equal(pn$eigenvalues, ref$eigenvalues)
    expect_equal(pn$eigenvectors, ref$eigenvectors)
    expect_equal(pn$loadings, ref$loadings)
    expect_equal(pn$eigenvalueThreshold, ref$eigenvalueThreshold)
    expect_equal(pn$edgeWeightThreshold, ref$edgeWeightThreshold)
    expect_equal(pn$components, ref$components)
    expect_equal(pn$scores, ref$scores)
    expect_equal(pn$componentGraphs[[1]]$getConnectedVertices(), ref$componentGraphs[[1]]$getConnectedVertices())
    expect_equal(pn$componentGraphs[[1]]$getEdgeDensity(), ref$componentGraphs[[1]]$getEdgeDensity())
    expect_equal(pn$residualGraphs[[1]]$getConnectedVertices(), ref$residualGraphs[[1]]$getConnectedVertices())
})
