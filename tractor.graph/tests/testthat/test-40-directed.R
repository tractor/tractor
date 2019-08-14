context("Fuzz (directed)")

source("properties.R")

graph <- randomGraph(10, M=60, directed=TRUE)

testMetricAgreement(graph)
