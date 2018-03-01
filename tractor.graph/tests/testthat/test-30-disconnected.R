context("Fuzz (disconnected)")

source("properties.R")

# Disconnected vertex
graph <- randomGraph(10, weights=runif(20))
graph[,1] <- 0
graph[1,] <- 0

testMetricAgreement(graph)

# Disjoint union of five-vertex subgraphs
graph <- randomGraph(10, weights=runif(20))
graph[1:5,6:10] <- 0
graph[6:10,1:5] <- 0

testMetricAgreement(graph)
