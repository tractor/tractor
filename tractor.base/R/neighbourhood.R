buildStepVectors <- function (width)
{
    smallRes <- width
    largeRes <- smallRes ^ 2
    maxStep <- smallRes %/% 2
    count <- smallRes ^ 3

    directions <- numeric(0)
    for (i in 0:(count-1))
    {
        x <- (i %/% largeRes) - maxStep
        y <- ((i %% largeRes) %/% smallRes) - maxStep
        z <- (i %% smallRes) - maxStep

        directions <- c(directions, x, y, z)
    }

    directionsArray <- array(directions, dim=c(3,count))
    return(directionsArray)
}

createNeighbourhoodInfo <- function (width, dim = 3, centre = rep(0,dim))
{
    if (dim != 3)
        report(OL$Error, "Only neighbourhoods in 3 dimensions are supported for now")
    
    vectors <- buildStepVectors(width) + centre
    
    unnormalised <- t(vectors) %*% vectors
    lengths <- sqrt(colSums(vectors^2))
    normalised <- unnormalised / (lengths %o% lengths)
    
    info <- list(width=width, dim=dim, centre=centre, vectors=vectors, innerProducts=normalised)
    class(info) <- "neighbourhoodInfo"
    invisible (info)
}
