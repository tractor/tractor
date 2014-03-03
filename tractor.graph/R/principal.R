.averageAndDecomposeGraphs <- function (graphs, setDiagonal = TRUE)
{
    nGraphs <- length(graphs)
    if (nGraphs == 1)
        meanConnectionMatrix <- graphs[[1]]$getAssociationMatrix()
    else
        meanConnectionMatrix <- Reduce("+", lapply(graphs, function(x) x$getAssociationMatrix())) / nGraphs
    
    if (setDiagonal)
        diag(meanConnectionMatrix) <- pmax(apply(meanConnectionMatrix,1,max,na.rm=TRUE), apply(meanConnectionMatrix,2,max,na.rm=TRUE))
    
    eigensystem <- eigen(meanConnectionMatrix)
    eigensystem$matrix <- meanConnectionMatrix
    
    return (eigensystem)
}

principalNetworks <- function (x, ...)
{
    UseMethod("principalNetworks")
}

# Table of raw data, e.g. cortical thickness measurements
principalNetworks.matrix <- function (x, ..., allVertexNames = NULL)
{
    graph <- asGraph(cor(x), directed=TRUE, allVertexNames=allVertexNames)
    principalGraphs <- calculatePrincipalGraphsForGraphs(graph, ...)
    principalGraphs$scores <- scale(x) %*% principalGraphs$eigenvectors
    
    return(principalGraphs)
}

# Single graph
principalNetworks.Graph <- function (x, ...)
{
    principalNetworks.list(list(x), ...)
}

# One or more graphs in a list
principalNetworks.list <- function (x, components = NULL, eigenvalueThreshold = NULL, loadingThreshold = 0.1, iterations = 0, confidence = 0.95, edgeWeightThreshold = 0.2, dropTrivial = TRUE, setDiagonal = TRUE)
{
    if (!is(x[[1]], "Graph"))
        report(OL$Error, "The specified list does not seem to contain Graph objects")
    if (length(x) == 1 && iterations > 0)
    {
        flag(OL$Warning, "Bootstrapping cannot be performed with only one graph")
        iterations <- 0
    }
    
    nGraphs <- length(x)
    eigensystem <- .averageAndDecomposeGraphs(x)
    associationMatrix <- eigensystem$matrix
    nComponents <- length(eigensystem$values)
    
    # Check for substantially negative eigenvalues
    if (any(eigensystem$values < -sqrt(.Machine$double.eps)))
        flag(OL$Warning, "Connection matrix is not positive semidefinite")
    
    # If no eigenvalue threshold is given, choose an "equal share" threshold
    if (is.null(eigenvalueThreshold))
        eigenvalueThreshold <- mean(eigensystem$values, na.rm=TRUE)
    
    if (iterations > 0)
    {
        report(OL$Info, "Bootstrapping loading matrix with threshold ", signif(loadingThreshold,3), " and confidence ", confidence)
        bootstrapResult <- bootstrapLoadings(x, iterations, loadingThreshold, eigenvalueThreshold, confidence)
        if (is.null(components))
            components <- which(bootstrapResult$values$salient)
        loadings <- structure(bootstrapResult$vectors$estimate, salient=bootstrapResult$vectors$salient)
    }
    else
    {
        report(OL$Info, "Using fixed loading threshold of ", signif(loadingThreshold,3))
        if (is.null(components))
            components <- which(eigensystem$values > eigenvalueThreshold)
        loadings <- structure(eigensystem$vectors, salient=abs(eigensystem$vectors)>loadingThreshold)
    }
    
    # Arrange for the eigenvector components to always sum to a positive value over salient elements
    for (i in seq_len(ncol(loadings)))
        loadings[,i] <- loadings[,i] * ifelse(sum(loadings[attr(loadings,"salient")[,i],i]) < 0, -1, 1)
    
    rownames(loadings) <- x[[1]]$getVertexAttributes("name")
    colnames(loadings) <- paste("PN", 1:nComponents, sep="")
    
    report(OL$Info, "Component graphs ", implode(components,sep=", ",finalSep=" and ",ranges=TRUE), " have eigenvalues above threshold")
    
    # Keep components which contain at least one connection between vertices
    if (dropTrivial)
    {
        nontrivial <- sapply(components, function(i) {
            m <- eigensystem$values[i] * (loadings[,i] %o% loadings[,i])
            v <- which(attr(loadings,"salient")[,i])
            m <- m[v,v]
            return (any(m >= edgeWeightThreshold & (upper.tri(m) | lower.tri(m))))
        })
        
        report(OL$Info, "Component graphs ", implode(components[!nontrivial],sep=", ",finalSep=" and ",ranges=TRUE), " are trivial and will be discarded")
        components <- components[nontrivial]
    }
    
    # Calculate the connection matrices for each component (including ones to be discarded later)
    fullMatrices <- lapply(1:nComponents, function(i) {
        m <- eigensystem$values[i] * (eigensystem$vectors[,i] %o% eigensystem$vectors[,i])
        m[is.na(associationMatrix)] <- NA
        rownames(m) <- rownames(associationMatrix)
        colnames(m) <- colnames(associationMatrix)
        return (m)
    })
    
    # Calculate residual association matrices after subtracting out higher components
    residualMatrices <- Reduce("-", fullMatrices, init=associationMatrix, accumulate=TRUE)
    residualMatrices <- residualMatrices[-1]
    residualGraphs <- lapply(residualMatrices[components], asGraph, allVertexNames=x[[1]]$getVertexAttributes("name"))
    residualGraphs <- lapply(residualGraphs, function(r) { r$setVertexLocations(x[[1]]$getVertexLocations(),x[[1]]$getVertexLocationUnit()); r })
    names(residualGraphs) <- paste("PN", components, sep="")
    
    verticesToKeep <- attr(loadings, "salient")
    matrices <- lapply(components, function(i) fullMatrices[[i]][verticesToKeep[,i],verticesToKeep[,i]])
    componentGraphs <- lapply(matrices, asGraph, allVertexNames=x[[1]]$getVertexAttributes("name"))
    componentGraphs <- lapply(componentGraphs, function(r) { r$setVertexLocations(x[[1]]$getVertexLocations(),x[[1]]$getVertexLocationUnit()); r })
    names(componentGraphs) <- paste("PN", components, sep="")
    
    loadings <- structure(loadings[,components], salient=attr(loadings,"salient")[,components])
    
    return (list(eigenvalues=eigensystem$values, eigenvectors=eigensystem$vectors, loadings=loadings, eigenvalueThreshold=eigenvalueThreshold, loadingThreshold=loadingThreshold, edgeWeightThreshold=edgeWeightThreshold, components=components, componentGraphs=componentGraphs, residualGraphs=residualGraphs, scores=NULL))
}

printLoadings <- function (loadings, threshold = 0.1, ignoreAttribute = FALSE)
{
    if (!is.matrix(loadings))
        report(OL$Error, "Loadings should be specified as a matrix")
    
    if (!ignoreAttribute && !is.null(attr(loadings,"salient")))
        loadings <- loadings * as.numeric(attr(loadings,"salient"))
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

bootstrapLoadings <- function (graphs, iterations = 1000, loadingThreshold = 0, eigenvalueThreshold = 1, confidence = 0.95)
{
    if (!is.list(graphs))
        report(OL$Error, "Graphs must be specified in a list")
    if (length(graphs) < 2)
        report(OL$Error, "At least two graphs must be specified")
    
    reference <- .averageAndDecomposeGraphs(graphs)
    allValues <- array(NA, dim=c(length(reference$values),iterations))
    allVectors <- array(NA, dim=c(dim(reference$vectors),iterations))
    
    for (i in 1:iterations)
    {
        sample <- sample(seq_along(graphs), replace=TRUE)
        current <- .averageAndDecomposeGraphs(graphs[sample])
        match <- matchLoadings(current$vectors, reference$vectors)
        allValues[,i] <- current$values[attr(match,"permutation")]
        allVectors[,,i] <- match
        
        if (i %% 100 == 0)
            report(OL$Verbose, "Done ", i)
    }
    
    quantiles <- c(0,confidence) + (1-confidence)/2
    
    limits <- apply(allValues, 1, quantile, quantiles, na.rm=TRUE, names=FALSE)
    salient <- limits[1,] > eigenvalueThreshold | limits[2,] < (-eigenvalueThreshold)
    values <- list(estimate=reference$values, limits=t(limits), salient=salient)
    
    limits <- apply(allVectors, 1:2, quantile, quantiles, na.rm=TRUE, names=FALSE)
    salient <- limits[1,,] > loadingThreshold | limits[2,,] < (-loadingThreshold)
    vectors <- list(estimate=reference$vectors, limits=aperm(limits,c(2,3,1)), salient=salient)
    
    return (list(values=values, vectors=vectors))
}
