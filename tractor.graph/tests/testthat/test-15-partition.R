context("Subnetworks")

reportr::setOutputLevel(Warning)

test_that("partitioning works as expected", {
    graph <- readGraphFile("graph")
    graph$binarise()
    
    membershipMatches <- function (a, b) {
        all(sapply(unique(a), function(x) allEqual(b[a==x])))
    }
    
    t_partitioned <- partitionGraph(graph, method="modularity")
    expect_equal(t_partitioned$nCommunities(), 3L)
    expect_true(all(t_partitioned$getVertexWeights() %in% 0:1))
    i_partitioned <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_true(membershipMatches(t_partitioned$getVertexMemberships(), i_partitioned$membership))
    expect_equal(modularity(t_partitioned), igraph::modularity(i_partitioned))
    
    graph <- randomGraph(10, M=20)
    t_partitioned <- partitionGraph(graph, method="modularity")
    i_partitioned <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_true(membershipMatches(t_partitioned$getVertexMemberships(), i_partitioned$membership))
    expect_equal(modularity(t_partitioned), igraph::modularity(i_partitioned))
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
