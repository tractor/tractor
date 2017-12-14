context("Fuzz (negative)")

source("metrics.R")

weights <- runif(20)
weights[1] <- -weights[1]
graph <- randomGraph(10, weights=weights, directed=TRUE)

testMetricAgreement(graph)
