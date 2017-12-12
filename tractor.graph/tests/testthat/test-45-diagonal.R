context("Fuzz (self-connected)")

source("metrics.R")

graph <- randomGraph(10, weights=runif(30), selfConnections=TRUE)

testMetricAgreement(graph)
