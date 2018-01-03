context("Basics")

reportr::setOutputLevel(Warning)

# Undirected ring graph, with five vertices and edges 1-2, 2-3, 3-4, 4-5 and 5-1
matrix <- matrix(c(0, 1, 0, 0, 1,
                   1, 0, 1, 0, 0,
                   0, 1, 0, 1, 0,
                   0, 0, 1, 0, 1,
                   1, 0, 0, 1, 0), nrow=5, ncol=5, byrow=TRUE)
names <- c("one", "two", "three", "four", "five")
dimnames(matrix) <- list(names, names)

graph <- asGraph(matrix)

test_that("an empty graph is handled sensibly", {
    emptyGraph <- Graph$new()
    expect_equal(emptyGraph$nVertices(), 0L)
    expect_equal(emptyGraph$nEdges(), 0L)
})

test_that("basic graph properties are correct", {
    expect_equal(graph$nVertices(), 5L)
    expect_equal(graph$nEdges(), 5L)
    expect_false(graph$isWeighted())
    expect_false(graph$isDirected())
    expect_false(graph$isSelfConnected())
    expect_equal(graph$getAssociationMatrix(), matrix)
    expect_equal(graph$getAdjacencyMatrix(), matrix)
    expect_equal(graph$getConnectedVertices(), graph$getVertices())
})

test_that("vertex attributes are handled properly", {
    expect_equal(graph$getVertexAttributes("name"), names)
    expect_equal(graph$getVertices(name %~% "^t"), 2:3)
})

test_that("computed graph properties are correct", {
    expect_equal(edgeDensity(graph), 0.5)
    expect_equivalent(vertexDegree(graph), rep(2,5L))
    expect_equal(neighbourhoods(graph), list(c(2,5),c(1,3),c(2,4),c(3,5),c(1,4)))
    expect_equal(meanShortestPath(graph), 1.5)
    expect_equal(clusteringCoefficients(graph), rep(0,5L))
    expect_equal(graphEfficiency(graph), 0.75)
    expect_equal(graphEfficiency(graph,"local"), rep(0,5L))
})

test_that("graph properties can be computed on a matrix", {
    expect_equal(edgeDensity(matrix), 0.5)
    expect_equivalent(vertexDegree(matrix), rep(2,5L))
    expect_equal(neighbourhoods(matrix), list(c(2,5),c(1,3),c(2,4),c(3,5),c(1,4)))
    expect_equal(meanShortestPath(matrix), 1.5)
    expect_equal(clusteringCoefficients(matrix), rep(0,5L))
    expect_equal(graphEfficiency(matrix), 0.75)
    expect_equal(graphEfficiency(matrix,"local"), rep(0,5L))
})

test_that("graph can be modified successfully", {
    graph$setEdgeAttributes(name=names)
    expect_equal(graph$getEdges(name %~% "^t"), matrix(c(2,3,3,4),2,2,byrow=TRUE))
    graph$setEdgeWeights(1:5)
    expect_equivalent(vertexStrength(graph), c(5,3,5,8,9))
    graph$map(fx(2*x))
    expect_equivalent(vertexStrength(graph), c(10,6,10,16,18))
    graph$binarise()
    expect_equivalent(vertexStrength(graph), rep(2,5L))
})
