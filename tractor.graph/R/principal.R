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
