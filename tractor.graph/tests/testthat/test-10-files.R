library(tractor.graph)
library(igraph)
context("files")

test_that("Graphs are saved and loaded successfully", {
    createSymMatrix <- function(vector, dim)
    {
        m <- matrix(rep(0,dim*dim), nrow=dim)
        m[lower.tri(m)] <- vector
        m <- t(m)
        m[lower.tri(m)] <- vector
        return(m)
    }
    vector <- sample(c(runif(20,0,2), rep(0,25)))
    m <- createSymMatrix(vector, 10)
    t_graph <- asGraph.matrix(m)
    volume <- runif(10, 0,5)
    voxelCount <- round(runif(10,0,20))
    names <- c("a","b","c","d","e","f","g","h","i","j")
    
    t_graph$setVertexAttributes("voxelCount"=voxelCount, "volume"=volume, "names"=names)
    fileName <- tempfile()
    writeGraphFile(t_graph, fileName, fileType = "csv")
    t_retrieved <- readGraphFile(fileName)
    expect_equivalent(t_retrieved, t_graph)
})
