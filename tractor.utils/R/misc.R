insertRowAt <- function (index, x, rowData)
{
    x <- promote(x)
    end <- nrow(x)
    
    if (index == 1)
        result <- rbind(rowData, x)
    else if (index == end+1)
        result <- rbind(x, rowData)
    else if (index > 1 && index <= end)
        result <- rbind(x[1:(index-1),,drop=FALSE], rowData, x[index:end,,drop=FALSE])
    else
        report(OL$Error, "Index (", index, ") is out of bounds")
    
    return (result)
}

insertColumnAt <- function (index, x, colData)
{
    x <- promote(x, byrow=TRUE)
    end <- ncol(x)
    
    if (index == 1)
        result <- cbind(colData, x)
    else if (index == end+1)
        result <- cbind(x, colData)
    else if (index > 1 && index <= end)
        result <- cbind(x[,1:(index-1),drop=FALSE], colData, x[,index:end,drop=FALSE])
    else
        report(OL$Error, "Index (", index, ") is out of bounds")
    
    return (result)
}

restrict <- function (x, fun = NULL, ..., invert = FALSE, na.rm=TRUE)
{
    if (!is.vector(x))
        report(OL$Error, "The \"restrict\" function only works for vectors")
    if (is.null(fun))
        matches <- rep(TRUE, length(x))
    else
    {
        fun <- match.fun(fun)
        matches <- fun(x, ...)
        if (!is.logical(matches))
            report(OL$Error, "The result of applying the specified function to the vector is not of logical type")
    }
    
    if (invert)
        matches <- !matches
    if (na.rm)
        matches <- matches & !is.na(x)
    
    return (x[matches])
}
