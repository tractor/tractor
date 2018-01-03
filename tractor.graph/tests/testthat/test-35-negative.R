context("Fuzz (negative)")

source("properties.R")

weights <- runif(20)
# All positive costs (inverse weights) must be at least as high in magnitude as this, to avoid negative loops
weights[1] <- -1
graph <- randomGraph(10, weights=weights, directed=TRUE)

testMetricAgreement(graph)
