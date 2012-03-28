calculatePrincipalGraphsForTable <- function (table, nComponents = NULL, loadingThreshold = 0.1, correlationThreshold = 0.2, ignoreCorrelationSign = FALSE, pureComponents = FALSE)
{
    # Scaling the data turns the covariances into correlations
    pcaResult <- prcomp(table, scale.=TRUE)
    
    varianceProportions <- (pcaResult$sdev^2) / sum(pcaResult$sdev^2)
    
    if (is.null(nComponents))
        nComponents <- sum(varianceProportions >= 1/length(varianceProportions))
    else
        nComponents <- min(nComponents, length(pcaResult$sdev))
    
    report(OL$Info, nComponents, " of ", length(varianceProportions), " components will be kept")
    
    verticesToKeep <- abs(pcaResult$rotation) >= loadingThreshold
    fullCorrelations <- lapply(1:nComponents, function (i) pcaResult$sdev[i]^2 * pcaResult$rotation[,i] %o% pcaResult$rotation[,i])
    
    # Calculate residual correlations after subtracting out higher components
    if (!pureComponents)
    {
        fullCorrelations <- Reduce("-", fullCorrelations, init=cor(table), accumulate=TRUE)
        fullCorrelations <- fullCorrelations[-length(fullCorrelations)]
    }
    
    correlations <- lapply(1:nComponents, function (i) fullCorrelations[[i]][verticesToKeep[,i],verticesToKeep[,i]])
    
    graphs <- lapply(correlations, newGraphFromConnectionMatrix, threshold=correlationThreshold, ignoreSign=ignoreCorrelationSign, allVertexNames=colnames(table))
    
    return (list(eigenvalues=pcaResult$sdev^2, eigenvectors=pcaResult$rotation, fullCorrelations=fullCorrelations, correlations=correlations, graphs=graphs))
}

calculatePrincipalGraphsForGraph <- function (graph, nComponents = NULL, loadingThreshold = 0.1)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "The specified graph is not a valid Graph object")
    
    connectionMatrix <- graph$getConnectionMatrix()
    connectionMatrix[is.na(connectionMatrix)] <- 0
    
    eigensystem <- eigen(connectionMatrix, symmetric=!graph$isDirected())
    
    # Check for substantially negative eigenvalues
    if (any(eigensystem$values < -sqrt(.Machine$double.eps)))
        flag(OL$Warning, "Connection matrix is not positive semidefinite")
    
    contributions <- eigensystem$values / sum(eigensystem$values)
    
    if (is.null(nComponents))
        nComponents <- sum(contributions >= 1/length(contributions))
    else
        nComponents <- min(nComponents, length(contributions))
    
    report(OL$Info, nComponents, " of ", length(contributions), " components will be kept")
    
    fullMatrices <- lapply(1:nComponents, function (i) {
        m <- eigensystem$values[i] * (eigensystem$vectors[,i] %o% eigensystem$vectors[,i])
        m[is.na(connectionMatrix)] <- NA
        rownames(m) <- rownames(connectionMatrix)
        colnames(m) <- colnames(connectionMatrix)
        return (m)
    })
    
    # Calculate residual association matrices after subtracting out higher components
    residualMatrices <- Reduce("-", fullMatrices, init=connectionMatrix, accumulate=TRUE)
    residualMatrices <- residualMatrices[-1]
    residualGraphs <- lapply(residualMatrices, newGraphFromConnectionMatrix, allVertexNames=graph$getVertexNames())
    
    verticesToKeep <- abs(eigensystem$vectors) >= loadingThreshold
    matrices <- lapply(1:nComponents, function (i) fullMatrices[[i]][verticesToKeep[,i],verticesToKeep[,i]])
    componentGraphs <- lapply(matrices, newGraphFromConnectionMatrix, allVertexNames=graph$getVertexNames())
    
    return (list(eigenvalues=eigensystem$values, eigenvectors=eigensystem$vectors, componentGraphs=componentGraphs, residualGraphs=residualGraphs))
}
