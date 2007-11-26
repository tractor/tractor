applyTransformation <- function (x, m)
{
    if (is.null(x))
        return (m)
    else
    {
        if (is.matrix(x))
        {
            x <- cbind(x,1)
            len <- ncol(x)
            result <- m %*% t(x)
            return (t(result[1:(len-1),]))
        }
        else if (is.vector(x))
        {
            x <- c(x,1)
            len <- length(x)
            result <- m %*% x
            return (drop(result[1:(len-1),]))
        }
    }
}

transformWithTranslation <- function (x, ...)
{
    translation <- c(..., 1)
    len <- length(translation)
    
    m <- diag(len)
    m[,len] <- translation
    
    return (applyTransformation(x, m))
}

transformWithScaling <- function (x, ...)
{
    scaling <- c(..., 1)
    m <- diag(scaling)
    
    return (applyTransformation(x, m))
}

transformRVoxelToFslVoxel <- function (x)
{
    if (is.matrix(x))
        len <- ncol(x)
    else if (is.vector(x))
        len <- length(x)
    return (transformWithTranslation(x, rep(-1,len)))
}

transformFslVoxelToRVoxel <- function (x)
{
    if (is.matrix(x))
        len <- ncol(x)
    else if (is.vector(x))
        len <- length(x)
    return (transformWithTranslation(x, rep(1,len)))
}

transformFslVoxelToWorld <- function (x, metadata, useOrigin = FALSE)
{
    if (useOrigin)
    {
        if (is.null(x))
        {
            m1 <- transformFslVoxelToRVoxel(NULL)
            m2 <- transformWithTranslation(NULL, -metadata$getOrigin())
            m3 <- transformWithScaling(NULL, metadata$getVoxelDimensions())
            return (m3 %*% m2 %*% m1)
        }
        else
        {
            rVoxel <- transformFslVoxelToRVoxel(x)
            zeroOrigin <- transformWithTranslation(rVoxel, -metadata$getOrigin())
            worldLoc <- transformWithScaling(zeroOrigin, metadata$getVoxelDimensions())
            return (worldLoc)
        }
    }
    else
        return (transformWithScaling(x, abs(metadata$getVoxelDimensions())))
}

transformWorldToFslVoxel <- function (x, metadata, useOrigin = FALSE)
{
    if (useOrigin)
    {
        if (is.null(x))
        {
            m1 <- transformWithScaling(NULL, 1/metadata$getVoxelDimensions())
            m2 <- transformWithTranslation(NULL, metadata$getOrigin())
            m3 <- transformRVoxelToFslVoxel(NULL)
            return (m3 %*% m2 %*% m1)
        }
        else
        {
            zeroOrigin <- transformWithScaling(x, 1/metadata$getVoxelDimensions())
            rVoxel <- transformWithTranslation(zeroOrigin, metadata$getOrigin())
            fslVoxel <- transformRVoxelToFslVoxel(rVoxel)
            return (fslVoxel)
        }
    }
    else
        return (transformWithScaling(x, abs(1/metadata$getVoxelDimensions())))
}

transformRVoxelToWorld <- function (x, metadata, useOrigin = FALSE)
{
    if (is.null(x))
    {
        m1 <- transformRVoxelToFslVoxel(NULL)
        m2 <- transformFslVoxelToWorld(NULL, metadata, useOrigin=useOrigin)
        return (m2 %*% m1)
    }
    else
    {
        fslVoxel <- transformRVoxelToFslVoxel(x)
        worldLoc <- transformFslVoxelToWorld(fslVoxel, metadata, useOrigin=useOrigin)
        return (worldLoc)
    }
}

transformWorldToRVoxel <- function (x, metadata, useOrigin = FALSE)
{
    if (is.null(x))
    {
        m1 <- transformWorldToFslVoxel(NULL, metadata, useOrigin=useOrigin)
        m2 <- transformFslVoxelToRVoxel(NULL)
        return (m2 %*% m1)
    }
    else
    {
        fslVoxel <- transformWorldToFslVoxel(x, metadata, useOrigin=useOrigin)
        rVoxel <- transformFslVoxelToRVoxel(fslVoxel)
        return (rVoxel)
    }
}
