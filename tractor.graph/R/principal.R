calculatePrincipalGraphsForTable <- function (table, ..., allVertexNames = NULL)
{
    graph <- newGraphFromTable(table, method="correlation", allVertexNames=allVertexNames)
    principalGraphs <- calculatePrincipalGraphsForGraph(graph, ...)
    principalGraphs$scores <- scale(table) %*% principalGraphs$eigenvectors
    
    return(principalGraphs)
}

calculatePrincipalGraphsForGraph <- function (graph, components = "vertices", loadingThreshold = "complete", rotation = c("none","varimax","promax","oblimin"), edgeWeightThreshold = 0.2, eigenvalueThreshold = NULL)
{
    if (!is(graph, "Graph"))
        report(OL$Error, "The specified graph is not a valid Graph object")
    
    rotation <- match.arg(rotation)
    if (is.character(components))
        components <- match.arg(components, c("eigenvalue","vertices"))
    if (is.character(loadingThreshold))
        loadingThreshold <- match.arg(loadingThreshold, "complete")
    
    connectionMatrix <- graph$getConnectionMatrix()
    
    eigensystem <- eigen(connectionMatrix, symmetric=!graph$isDirected())
    nComponents <- length(eigensystem$values)
    
    # Check for substantially negative eigenvalues
    if (any(eigensystem$values < -sqrt(.Machine$double.eps)))
        flag(OL$Warning, "Connection matrix is not positive semidefinite")
    
    # Rotate the loading matrix if requested
    loadings <- eigensystem$vectors
    loadings <- switch(rotation, none=loadings,
                                 varimax=varimax(loadings,normalize=FALSE)$loadings,
                                 promax=promax(loadings)$loadings,
                                 oblimin={ require("GPArotation"); oblimin(loadings)$loadings })
    
    # Arrange for the eigenvector components to always sum to a positive value
    loadings <- apply(loadings, 2, function(x) x * ifelse(sum(x)<0,-1,1))
    
    rownames(loadings) <- graph$getVertexAttributes("names")
    colnames(loadings) <- paste("PN", 1:nComponents, sep="")
    
    # Choosing the components to keep and loading threshold can be an interative process
    iteration <- 1
    prevComponents <- 1:nComponents
    prevLoadingThreshold <- Inf    
    repeat
    {
        if (is.numeric(components))
            components <- intersect(components, 1:nComponents)
        else if (components == "eigenvalue")
        {
            # If no eigenvalue threshold is given, choose an "equal share" threshold
            if (is.null(eigenvalueThreshold))
            {
                contributions <- eigensystem$values / sum(eigensystem$values)
                components <- which(contributions >= 1/length(contributions))
            }
            else
                components <- which(eigensystem$values >= eigenvalueThreshold)
        }
        else if (components == "vertices")
        {
            if (is.numeric(loadingThreshold))
                tempLoadingThreshold <- loadingThreshold
            else if (!exists("tempLoadingThreshold"))
                tempLoadingThreshold <- prevLoadingThreshold
            
            # Keep components which contain at least one connection between vertices
            tempComponents <- which(sapply(1:nComponents, function(i) {
                m <- eigensystem$values[i] * (loadings[,i] %o% loadings[,i])
                v <- which(abs(loadings[,i]) >= tempLoadingThreshold)
                m <- m[v,v]
                return (any(m >= edgeWeightThreshold & (upper.tri(m) | lower.tri(m))))
            }))
            
            if (identical(tempComponents, prevComponents))
                components <- tempComponents
            else
                prevComponents <- tempComponents
        }
        
        if (loadingThreshold == "complete")
        {
            if (is.numeric(components))
                tempComponents <- components
            
            # Choose the largest loading threshold which will allow each vertex to appear in at least one PN
            tempLoadingThreshold <- min(apply(abs(loadings[,tempComponents]), 1, max))
            if (identical(tempLoadingThreshold, prevLoadingThreshold))
                loadingThreshold <- tempLoadingThreshold
            else
                prevLoadingThreshold <- tempLoadingThreshold
        }
        
        # Check for convergence
        if (is.numeric(loadingThreshold) && is.numeric(components))
            break
        else
        {
            report(OL$Debug, "Component graphs on loop ", iteration, " are ", implode(tempComponents,sep=", ",finalSep=" and "))
            report(OL$Debug, "Loading threshold on loop ", iteration, " is ", signif(tempLoadingThreshold,3))
            iteration <- iteration + 1
        }
    }
    
    report(OL$Info, "Retaining component graphs ", implode(components,sep=", ",finalSep=" and "))
    report(OL$Info, "Loading threshold is ", signif(loadingThreshold,3))
    
    # Calculate the connection matrices for each component (including ones to be discarded later)
    fullMatrices <- lapply(1:nComponents, function(i) {
        m <- eigensystem$values[i] * (loadings[,i] %o% loadings[,i])
        m[is.na(connectionMatrix)] <- NA
        rownames(m) <- rownames(connectionMatrix)
        colnames(m) <- colnames(connectionMatrix)
        return (m)
    })
    
    # Calculate residual association matrices after subtracting out higher components
    residualMatrices <- Reduce("-", fullMatrices, init=connectionMatrix, accumulate=TRUE)
    residualMatrices <- residualMatrices[-1]
    residualGraphs <- lapply(residualMatrices[components], newGraphFromConnectionMatrix, allVertexNames=graph$getVertexAttributes()$names)
    residualGraphs <- lapply(residualGraphs, function(x) { x$setVertexLocations(graph$getVertexLocations(),graph$getVertexLocationUnit()); x })
    names(residualGraphs) <- paste("PN", components, sep="")
    
    verticesToKeep <- abs(loadings) >= loadingThreshold
    matrices <- lapply(components, function(i) fullMatrices[[i]][verticesToKeep[,i],verticesToKeep[,i]])
    componentGraphs <- lapply(matrices, newGraphFromConnectionMatrix, allVertexNames=graph$getVertexAttributes()$names)
    componentGraphs <- lapply(componentGraphs, function(x) { x$setVertexLocations(graph$getVertexLocations(),graph$getVertexLocationUnit()); x })
    names(componentGraphs) <- paste("PN", components, sep="")
    
    return (list(eigenvalues=eigensystem$values, eigenvectors=loadings, loadingThreshold=loadingThreshold, edgeWeightThreshold=edgeWeightThreshold, rotation=rotation, components=components, componentGraphs=componentGraphs, residualGraphs=residualGraphs, scores=NULL))
}

printLoadings <- function (loadings, threshold = 0.1)
{
    if (!is.matrix(loadings))
        report(OL$Error, "Loadings should be specified as a matrix")
    
    loadings[abs(loadings) < threshold] <- 0
    nonzero <- rowSums(loadings != 0)
    loadings <- loadings[nonzero>0,]
    loadingList <- lapply(1:ncol(loadings), function(i) abs(loadings[,i]))
    order <- do.call("order", c(loadingList,list(decreasing=TRUE)))
    printSparse(loadings[order,])
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
