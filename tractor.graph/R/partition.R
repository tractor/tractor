partitionGraph <- function (graph, method = "modularity")
{
    if (!is(graph, "Graph"))
        report(OL$Error, "The specified graph is not a valid Graph object")
    
    method <- match.arg(method)
    
    if (method == "modularity")
    {
        # Following Newman (PNAS, 2006)
        adjacencyMatrix <- ifelse(graph$getAssociationMatrix() != 0, 1, 0)
        diag(adjacencyMatrix) <- diag(adjacencyMatrix) * 2
        degree <- graph$getVertexDegree()
        nEdges <- graph$nEdges()
        modularityMatrix <- adjacencyMatrix - (degree %o% degree) / (2 * nEdges)
        
        findPartition <- function (indices)
        {
            modularitySubmatrix <- modularityMatrix[indices,indices,drop=FALSE]
            diag(modularitySubmatrix) <- diag(modularitySubmatrix) - rowSums(modularitySubmatrix)
            eigensystem <- eigen(modularitySubmatrix)
            
            groupMembership <- sign(sign(eigensystem$vectors[,1]) + 0.5)
            firstGroupIndices <- indices[which(groupMembership < 0)]
            secondGroupIndices <- indices[which(groupMembership >= 0)]
            modularityIncrease <- (matrix(groupMembership,nrow=1) %*% modularitySubmatrix %*% matrix(groupMembership,ncol=1)) / (4 * nEdges)
            
            if (length(firstGroupIndices) > 0 && length(secondGroupIndices) > 0 && modularityIncrease > 0)
                return (c(findPartition(firstGroupIndices), findPartition(secondGroupIndices)))
            else
                return (list(indices))
        }
        
        partition <- findPartition(graph$getConnectedVertices())
        report(OL$Info, "Graph has been partitioned into #{length(partition)} parts, containing #{implode(sapply(partition,length),sep=', ',finalSep=' and ')} vertices")
        return (partition)
    }
}
