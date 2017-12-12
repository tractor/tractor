context("Fuzz (disconnected)")

source("metrics.R")

graph <- randomGraph(10, weights=runif(20))
graph[,1] <- 0
graph[1,] <- 0

testMetricAgreement(graph)
