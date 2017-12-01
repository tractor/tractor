library(tractor.graph)
library(igraph)
context("files")

test_that("Partitioning works fine", {
    createSymMatrix <- function(vector, dim)
    {
        m <- matrix(rep(0,dim*dim), nrow=dim)
        m[lower.tri(m)] <- vector
        m <- t(m)
        m[lower.tri(m)] <- vector
        return(m)
    }
    vector <- sample(c(rep(1,20), rep(0,25)))
    m <- createSymMatrix(vector, 10)
    t_graph <- asGraph.matrix(m)
    i_graph <- as(t_graph, "igraph")
    t_partitioning <- partitionGraph(t_graph)
    i_partitioning <- cluster_leading_eigen(i_graph)
    
    matchList <- rep(NA, length(t_partitioning))
    for(i in 1:length(t_partitioning)){
        vector <- i_partitioning[[i]]
        matchList[i] <- which(lapply(t_partitioning, function(i) all(match(i, vector))) == TRUE)
    }
    expect_false(anyNA(matchList))
    expect_true(length(matchList)==length(unique(matchList)))
})
