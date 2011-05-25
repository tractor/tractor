resolveVector <- function (len, ...)
{
    vector <- c(...)
    if (is.numeric(vector) && (length(vector) == len))
        return (vector)
    else
        return (NULL)
}

vectorLength <- function (vector)
{
    return (sqrt(sum(vector^2)))
}

vectorCrossProduct <- function (a, b)
{
    if (length(a) != 3 || length(b) != 3)
        report(OL$Error, "Cross product is currently only defined for 3-vectors")
    
    # Ref: http://mathworld.wolfram.com/CrossProduct.html
    return (c(a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]))
}

angleBetweenVectors <- function (v1, v2)
{
    if (is.na(v1) || is.na(v2))
        return (NA)
    else
    {
        if (identical(v1,v2))
            cosine <- 1
        else
            cosine <- (v1 %*% v2) / (vectorLength(v1) * vectorLength(v2))
        return (acos(cosine))
    }
}
