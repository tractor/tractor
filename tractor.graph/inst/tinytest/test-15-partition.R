reportr::setOutputLevel(Warning)

graph <- readGraphFile("graph")

membershipMatches <- function (a, b) {
    all(sapply(unique(a), function(x) tractor.base::allEqual(b[a==x])))
}

# Weighted version (as stored)
# igraph produces a slightly different partition in this case, but the modularity is very similar
expect_equal(modularity(graph), 0)
t_partitioned <- partitionGraph(graph, method="modularity")
expect_true(t_partitioned$nCommunities() > 10)
expect_true(all(t_partitioned$getVertexWeights() %in% 0:1))
expect_equal_to_reference(modularityMatrix(graph), "modmat.rds")
expect_true(modularity(t_partitioned) >= 0)
expect_true(modularity(t_partitioned) <= 1)

if (requireNamespace("igraph", quietly=TRUE))
{
    i_partitioned <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_equal(modularity(t_partitioned), igraph::modularity(i_partitioned), tolerance=1e-3)
}

# Binarised version
graph$binarise()
t_partitioned <- partitionGraph(graph, method="modularity")
expect_equal(t_partitioned$nCommunities(), 3L)
expect_true(all(t_partitioned$getVertexWeights() %in% 0:1))

if (requireNamespace("igraph", quietly=TRUE))
{
    i_partitioned <- igraph::cluster_leading_eigen(as(graph, "igraph"))
    expect_true(membershipMatches(t_partitioned$getVertexMemberships(), i_partitioned$membership))
    expect_equal(modularity(t_partitioned), igraph::modularity(i_partitioned))
}

graph <- readGraphFile("graph")
pn <- principalNetworks(graph)$graph$serialise()
ref <- tractor.base::deserialiseReferenceObject("principal.Rdata", raw=TRUE)

expect_equal(abs(pn$vertexWeights), abs(ref$vertexWeights))
expect_equal(pn$communityWeights, ref$communityWeights)
