context("Fuzz (directed)")

source("metrics.R")

graph <- randomGraph(10, M=60, directed=TRUE)

testMetricAgreement(graph)
