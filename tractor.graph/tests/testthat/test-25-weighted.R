context("Fuzz (weighted)")

source("properties.R")

graph <- randomGraph(10, weights=runif(20))

testMetricAgreement(graph)
