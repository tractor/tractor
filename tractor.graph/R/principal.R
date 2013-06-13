calculatePrincipalGraphsForTable <- function (table, nComponents = NULL, loadingThreshold = 0.1, allVertexNames = NULL)
{
    graph <- newGraphFromTable(table, method="correlation", allVertexNames=allVertexNames)
    principalGraphs <- calculatePrincipalGraphsForGraph(graph, nComponents=nComponents, loadingThreshold=loadingThreshold)
    principalGraphs$scores <- scale(table) %*% principalGraphs$eigenvectors
    
    return(principalGraphs)
}

calculatePrincipalGraphsForGraph <- function (graph, nComponents = NULL, loadingThreshold = 0.1)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "The specified graph is not a valid Graph object")
    
    connectionMatrix <- graph$getConnectionMatrix()
    
    eigensystem <- eigen(connectionMatrix, symmetric=!graph$isDirected())
    
    # Check for substantially negative eigenvalues
    if (any(eigensystem$values < -sqrt(.Machine$double.eps)))
        flag(OL$Warning, "Connection matrix is not positive semidefinite")
    
    # Arrange for the eigenvector components to always sum to a positive value
    # The sign function is used twice to ensure that the value is either +1 or -1, and not zero
    eigensystem$vectors <- apply(eigensystem$vectors, 2, function (x) x * sign(sign(sum(x))+0.5))
    
    if (is.null(nComponents))
    {
        contributions <- eigensystem$values / sum(eigensystem$values)
        nComponents <- sum(contributions >= 1/length(contributions))
    }
    else
        nComponents <- min(nComponents, length(eigensystem$values))
    
    report(OL$Info, nComponents, " of ", length(eigensystem$values), " components will be kept")
    
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
    residualGraphs <- lapply(residualMatrices, newGraphFromConnectionMatrix, allVertexNames=graph$getVertexAttributes()$names)
    residualGraphs <- lapply(residualGraphs, function (x) { x$setVertexLocations(graph$getVertexLocations(),graph$getVertexLocationUnit()); x })
    
    verticesToKeep <- abs(eigensystem$vectors) >= loadingThreshold
    matrices <- lapply(1:nComponents, function (i) fullMatrices[[i]][verticesToKeep[,i],verticesToKeep[,i]])
    componentGraphs <- lapply(matrices, newGraphFromConnectionMatrix, allVertexNames=graph$getVertexAttributes()$names)
    componentGraphs <- lapply(componentGraphs, function (x) { x$setVertexLocations(graph$getVertexLocations(),graph$getVertexLocationUnit()); x })
    
    return (list(eigenvalues=eigensystem$values, eigenvectors=eigensystem$vectors, componentGraphs=componentGraphs, residualGraphs=residualGraphs, scores=NULL))
}

calculatePrincipalGraphsForGraph <- function (graph, nComponents = NULL, loadingThreshold = 0.1, weightThreshold = 0.2, ignoreWeightSign = FALSE)
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
    
    # Calculate cumulative association matrices after subtracting out higher components
    cumulativeMatrices <- Reduce("-", fullMatrices, init=connectionMatrix, accumulate=TRUE)
    cumulativeMatrices <- cumulativeMatrices[-length(cumulativeMatrices)]
    
    verticesToKeep <- abs(eigensystem$vectors) >= loadingThreshold
    matrices <- lapply(1:nComponents, function (i) fullMatrices[[i]][verticesToKeep[,i],verticesToKeep[,i]])
    graphs <- lapply(matrices, newGraphFromConnectionMatrix, threshold=weightThreshold, ignoreSign=ignoreWeightSign, allVertexNames=graph$getVertexNames())
    
    return (list(eigenvalues=eigensystem$values, eigenvectors=eigensystem$vectors, graphs=graphs, matrices=fullMatrices, cumulativeMatrices=cumulativeMatrices))
}
