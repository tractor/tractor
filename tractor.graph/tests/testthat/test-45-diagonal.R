context("Fuzz (self-connected)")

source("metrics.R")

graph <- randomGraph(10, M=30, selfConnections=TRUE)

testMetricAgreement(graph)
