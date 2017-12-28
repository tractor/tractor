principalNetworks <- function (x, ...)
{
    UseMethod("principalNetworks")
}

# Table of raw data, e.g. cortical thickness measurements
principalNetworks.matrix <- function (x, ...)
{
    graph <- asGraph(cor(x), edgeList=FALSE, directed=FALSE)
    principalGraphs <- principalNetworks.list(list(graph), ...)
    principalGraphs$scores <- scale(x) %*% principalGraphs$eigenvectors
    
    return(principalGraphs)
}

# Single graph
principalNetworks.Graph <- function (x, ...)
{
    principalNetworks.list(list(x), ...)
}

# One or more graphs in a list
principalNetworks.list <- function (x, components = NULL, replicates = 0L, reference = NULL)
{
    if (length(x) == 1 && replicates > 0)
    {
        flag(OL$Warning, "Bootstrapping cannot be performed with only one graph")
        replicates <- 0
    }
    
    nGraphs <- length(x)
    if (!is.null(reference))
        reference <- asPartitionedGraph(reference)
    if (is.null(components))
    {
        if (is.null(reference))
            components <- x[[1]]$nVertices()
        else
            components <- reference$nCommunities()
    }
    components <- seq_len(components)
    nComponents <- length(components)
    
    meanAssociationMatrix <- Reduce("+", lapply(x,as.matrix)) / nGraphs
    eigensystem <- eigen(meanAssociationMatrix)
    
    # Check for substantially negative eigenvalues
    if (any(eigensystem$values < -sqrt(.Machine$double.eps)))
        flag(OL$Warning, "Connection matrix is not positive semidefinite")
    
    if (is.null(reference))
        permutation <- components
    else
    {
        match <- matchLoadings(eigensystem$vectors[,components], reference$getVertexWeights()[,components])
        permutation <- attr(match, "permutation")
    }
    
    partitionedGraph <- PartitionedGraph$new(asGraph(meanAssociationMatrix), vertexWeights=eigensystem$vectors[,components[permutation]], communityWeights=eigensystem$values[components[permutation]])
    result <- list(graph=partitionedGraph)
    
    if (replicates > 0)
    {
        bootstrap <- boot::boot(x, function (data,indices) {
            current <- principalNetworks.list(data[indices], components=nComponents, reference=result$graph)
            return (c(partitionedGraph$getCommunityWeights(), as.vector(partitionedGraph$getVertexWeights())))
        }, replicates)
        
        # Bootstrapped community and vertex weights
        result$bcw <- bootstrap$t[,components]
        result$bvw <- structure(bootstrap$t[,-components], dim=c(replicates,nrow(meanAssociationMatrix),nComponents))
    }
    
    return (result)
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
