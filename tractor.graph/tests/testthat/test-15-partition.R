context("Subnetworks")

reportr::setOutputLevel(Warning)

test_that("partitioning works as expected", {
    graph <- readGraphFile("graph")
    graph$map(function(x) ifelse(x==0,0L,1L))
    
    membershipMatches <- function (l, v) {
        all(sapply(l, function(i) allEqual(v[i])))
    }
    
    t_partition <- partitionGraph(graph)
    i_partition <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_true(membershipMatches(t_partition, i_partition$membership))
    
    graph <- randomGraph(10, M=20)
    t_partition <- partitionGraph(graph)
    i_partition <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_true(membershipMatches(t_partition, i_partition$membership))
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
