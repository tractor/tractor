context("Fuzz (binary)")

source("metrics.R")

graph <- randomGraph(10, M=20)

testMetricAgreement(graph)
