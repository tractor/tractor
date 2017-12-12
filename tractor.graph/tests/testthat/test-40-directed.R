context("Fuzz (directed)")

source("metrics.R")

graph <- randomGraph(10, weights=runif(60), directed=TRUE)

testMetricAgreement(graph)
