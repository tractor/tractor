context("Files")

reportr::setOutputLevel(Warning)

test_that("graphs are saved and loaded successfully", {
    vertexAttribNames <- c("voxelCount", "volume", "name")
    graph <- readGraphFile("graph")
    
    expect_false(graph$isDirected())
    expect_true(graph$isWeighted())
    expect_true(graph$isSelfConnected())
    expect_equal(round(edgeDensity(graph)*100), 62)
    expect_true(all(vertexAttribNames %in% names(graph$getVertexAttributes())))
    
    fileName <- tempfile()
    writeGraphFile(graph, fileName, fileType="csv")
    retrievedGraph <- readGraphFile(fileName)
    
    expect_false(retrievedGraph$isDirected())
    expect_true(retrievedGraph$isWeighted())
    expect_true(retrievedGraph$isSelfConnected())
    expect_equal(round(edgeDensity(retrievedGraph)*100), 62)
    expect_true(all(vertexAttribNames %in% names(retrievedGraph$getVertexAttributes())))
    
    expect_equivalent(graph$getAssociationMatrix(), retrievedGraph$getAssociationMatrix())
})
