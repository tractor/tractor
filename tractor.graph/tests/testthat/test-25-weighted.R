context("Fuzz (weighted)")

source("metrics.R")

graph <- randomGraph(10, weights=runif(20))

testMetricAgreement(graph)
