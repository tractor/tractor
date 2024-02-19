context("Fuzz (self-connected)")

source("properties.R")

graph <- randomGraph(10, M=30, selfConnections=TRUE)

# igraph 2.0.x currently seems to drop self-connections from the adjacency matrix
if (packageVersion("igraph") < "2.0")
    testMetricAgreement(graph)
