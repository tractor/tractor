calculatePrincipalGraphsForTable <- function (table, ..., allVertexNames = NULL)
{
    graph <- newGraphFromTable(table, method="correlation", allVertexNames=allVertexNames)
    principalGraphs <- calculatePrincipalGraphsForGraph(graph, ...)
    principalGraphs$scores <- scale(table) %*% principalGraphs$eigenvectors
    
    return(principalGraphs)
}

calculatePrincipalGraphsForGraphs <- function (graphs, components = "vertices", loadingThreshold = 0.1, iterations = 1, confidence = 0.95, rotation = c("none","varimax","promax","oblimin"), edgeWeightThreshold = 0.2, eigenvalueThreshold = NULL)
{
    if (is(graphs, "Graph"))
    {
        if (iterations > 1)
        {
            flag(OL$Warning, "Bootstrapping cannot be performed with only one graph")
            iterations <- 1
        }
        graphs <- list(graphs)
    }
    else if (!is.list(graphs))
        report(OL$Error, "Graphs should be specified in a list")
    
    rotation <- match.arg(rotation)
    
    # The "components" option can also be a literal integer vector
    if (is.character(components))
        components <- match.arg(components, c("eigenvalue","vertices"))
    
    nGraphs <- length(graphs)
    if (nGraphs == 1)
        connectionMatrix <- graphs[[1]]$getConnectionMatrix()
    else
        connectionMatrix <- Reduce("+", lapply(graphs, function(x) x$getConnectionMatrix())) / nGraphs
    
    eigensystem <- eigen(connectionMatrix)
    nComponents <- length(eigensystem$values)
    
    # Check for substantially negative eigenvalues
    if (any(eigensystem$values < -sqrt(.Machine$double.eps)))
        flag(OL$Warning, "Connection matrix is not positive semidefinite")
    
    if (iterations > 1)
    {
        report(OL$Info, "Bootstrapping loading matrix with threshold ", signif(loadingThreshold,3), " and confidence ", confidence)
        bootstrapResult <- bootstrapLoadings(graphs, iterations, loadingThreshold, confidence)
        loadings <- structure(bootstrapResult$estimate, salience=bootstrapResult$significance)
    }
    else
    {
        report(OL$Info, "Using fixed loading threshold of ", signif(loadingThreshold,3))
        loadings <- structure(eigensystem$vectors, salience=abs(loadings)>loadingThreshold)
    }
    
    # Arrange for the eigenvector components to always sum to a positive value
    for (i in seq_len(ncol(loadings)))
        loadings[,i] <- loadings[,i] * ifelse(loadings[,i] < 0, -1, 1)
    
    rownames(loadings) <- graphs[[1]]$getVertexAttributes("names")
    colnames(loadings) <- paste("PN", 1:nComponents, sep="")
    
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
        # Keep components which contain at least one connection between vertices
        components <- which(sapply(1:nComponents, function(i) {
            m <- eigensystem$values[i] * (loadings[,i] %o% loadings[,i])
            v <- which(attr(loadings,"salience")[,i])
            m <- m[v,v]
            return (any(m >= edgeWeightThreshold & (upper.tri(m) | lower.tri(m))))
        }))
    }
    
    report(OL$Info, "Retaining component graphs ", implode(components,sep=", ",finalSep=" and "))
    
    # Rotate the loading matrix if requested
    rotatedLoadings <- switch(rotation, none=loadings[,components],
                                        varimax=varimax(loadings[,components],normalize=FALSE)$loadings,
                                        promax=promax(loadings[,components])$loadings,
                                        oblimin={ require("GPArotation"); oblimin(loadings[,components])$loadings })
    
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
    residualGraphs <- lapply(residualMatrices[components], newGraphFromConnectionMatrix, allVertexNames=graphs[[1]]$getVertexAttributes()$names)
    residualGraphs <- lapply(residualGraphs, function(x) { x$setVertexLocations(graphs[[1]]$getVertexLocations(),graphs[[1]]$getVertexLocationUnit()); x })
    names(residualGraphs) <- paste("PN", components, sep="")
    
    verticesToKeep <- attr(loadings, "salience")
    matrices <- lapply(components, function(i) fullMatrices[[i]][verticesToKeep[,i],verticesToKeep[,i]])
    componentGraphs <- lapply(matrices, newGraphFromConnectionMatrix, allVertexNames=graphs[[1]]$getVertexAttributes()$names)
    componentGraphs <- lapply(componentGraphs, function(x) { x$setVertexLocations(graphs[[1]]$getVertexLocations(),graphs[[1]]$getVertexLocationUnit()); x })
    names(componentGraphs) <- paste("PN", components, sep="")
    
    return (list(eigenvalues=eigensystem$values, eigenvectors=loadings, loadings=rotatedLoadings, loadingThreshold=loadingThreshold, edgeWeightThreshold=edgeWeightThreshold, rotation=rotation, components=components, componentGraphs=componentGraphs, residualGraphs=residualGraphs, scores=NULL))
}

printLoadings <- function (loadings, threshold = 0.1, ignoreAttribute = FALSE)
{
    if (!is.matrix(loadings))
        report(OL$Error, "Loadings should be specified as a matrix")
    
    if (!ignoreAttribute && !is.null(attr(loadings,"salience")))
        loadings <- loadings * as.numeric(attr(loadings,"salience"))
    else
        loadings[abs(loadings) < threshold] <- 0
    
    nonzero <- rowSums(loadings != 0)
    loadings <- loadings[nonzero>0,]
    loadingList <- lapply(1:ncol(loadings), function(i) abs(loadings[,i]))
    order <- do.call("order", c(loadingList,list(decreasing=TRUE)))
    printSparse(loadings[order,])
}

matchLoadings <- function (newLoadings, refLoadings)
{
    if (!is.matrix(newLoadings) || !is.matrix(refLoadings))
        report(OL$Error, "Loadings should be specified as matrices")
    if (nrow(newLoadings) != nrow(refLoadings))
        report(OL$Error, "Lengths of the loading vectors do not match")
    if (ncol(newLoadings) != ncol(refLoadings))
        report(OL$Error, "The number of components should be the same in both loading matrices")
    
    nComponents <- ncol(newLoadings)
    
    newLengths <- apply(newLoadings, 2, vectorLength)
    refLengths <- apply(refLoadings, 2, vectorLength)
    
    componentsLeft <- seq_len(ncol(newLoadings))
    permutation <- rep(NA, nComponents)
    finalCosines <- rep(NA, nComponents)
    for (i in 1:nComponents)
    {
        cosines <- sapply(1:ncol(newLoadings), function(j) {
            if (!any(componentsLeft == j))
                return (0)
            else
                return ((newLoadings[,j] %*% refLoadings[,i]) / (newLengths[j] * refLengths[i]))
        })
        
        maxAbsoluteCosine <- max(abs(cosines))
        index <- which.max(abs(cosines))
        if (maxAbsoluteCosine > 0)
        {
            permutation[i] <- index
            finalCosines[i] <- cosines[index]
            componentsLeft <- setdiff(componentsLeft, index)
        }
    }
    
    finalLoadings <- newLoadings[,permutation]
    toNegate <- which(finalCosines < 0)
    finalLoadings[,toNegate] <- (-finalLoadings[,toNegate])
    
    return (structure(finalLoadings, permutation=permutation, cosines=abs(finalCosines)))
}

bootstrapLoadings <- function (graphs, iterations = 1000, threshold = 0, confidence = 0.95)
{
    getLoadings <- function (graphs)
    {
        nGraphs <- length(graphs)
        meanConnectionMatrix <- Reduce("+", lapply(graphs, function(x) x$getConnectionMatrix())) / nGraphs
        loadings <- eigen(meanConnectionMatrix)$vectors
        return (loadings)
    }

    if (!is.list(graphs))
        report(OL$Error, "Graphs must be specified in a list")
    if (length(graphs) < 2)
        report(OL$Error, "At least two graphs must be specified")
    
    refLoadings <- getLoadings(graphs)
    allLoadings <- array(NA, dim=c(dim(refLoadings),iterations))
    
    for (i in 1:iterations)
    {
        sample <- sample(seq_along(graphs), replace=TRUE)
        allLoadings[,,i] <- matchLoadings(getLoadings(graphs[sample]), refLoadings)
        
        if (i %% 100 == 0)
            report(OL$Verbose, "Done ", i)
    }
    
    quantiles <- c(0,confidence) + (1-confidence)/2
    limits <- apply(allLoadings, 1:2, quantile, quantiles, na.rm=TRUE, names=FALSE)
    limits <- aperm(limits, c(2,3,1))
    
    significance <- limits[,,1] > threshold | limits[,,2] < (-threshold)
    
    return (list(estimate=refLoadings, limits=limits, significance=significance))
}
