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
    pn <- principalNetworks(graph)$graph$serialise()
    ref <- deserialiseReferenceObject("principal.Rdata", raw=TRUE)
    
    expect_equal(abs(pn$vertexWeights), abs(ref$vertexWeights))
    expect_equal(pn$communityWeights, ref$communityWeights)
})
