calculatePrincipalGraphsForTable <- function (table, nComponents = NULL, loadingThreshold = 0.1, correlationThreshold = 0.2, ignoreCorrelationSign = FALSE)
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
    correlations <- lapply(1:nComponents, function (i) pcaResult$sdev[i]^2 * pcaResult$rotation[verticesToKeep[,i],i] %o% pcaResult$rotation[verticesToKeep[,i],i])
    
    graphs <- lapply(correlations, newGraphFromConnectionMatrix, threshold=correlationThreshold, ignoreSign=ignoreCorrelationSign, allVertexNames=colnames(table))
    
    return (list(eigenvalues=pcaResult$sdev^2, eigenvectors=pcaResult$rotation, correlations=correlations, graphs=graphs))
}
