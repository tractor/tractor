context("Fuzz (binary)")

source("properties.R")

graph <- randomGraph(10, M=20)

testMetricAgreement(graph)
