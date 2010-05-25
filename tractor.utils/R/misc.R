insertRowAt <- function (index, x, rowData)
{
    require(tractor.base)
    
    x <- promote(x)
    end <- nrow(x)
    row <- matrix(rowData, nrow=1)
    
    if (index == 1)
        result <- rbind(row, x)
    else if (index == end+1)
        result <- rbind(x, row)
    else if (index > 1 && index <= end)
        result <- rbind(x[1:(index-1),,drop=FALSE], row, x[index:end,,drop=FALSE])
    else
        output(OL$Error, "Index (", index, ") is out of bounds")
    
    return (result)
}

insertColumnAt <- function (index, x, colData)
{
    require(tractor.base)
    
    x <- promote(x, byrow=TRUE)
    end <- ncol(x)
    col <- matrix(colData, ncol=1)
    
    if (index == 1)
        result <- cbind(col, x)
    else if (index == end+1)
        result <- cbind(x, col)
    else if (index > 1 && index <= end)
        result <- cbind(x[,1:(index-1),drop=FALSE], col, x[,index:end,drop=FALSE])
    else
        output(OL$Error, "Index (", index, ") is out of bounds")
    
    return (result)
}

restrict <- function (x, fun = NULL, ..., invert = FALSE, na.rm=TRUE)
{
    require(tractor.base)
    
    if (!is.vector(x))
        output(OL$Error, "The \"restrict\" function only works for vectors")
    if (is.null(fun))
        matches <- rep(TRUE, length(x))
    else
    {
        fun <- match.fun(fun)
        matches <- fun(x, ...)
        if (!is.logical(matches))
            output(OL$Error, "The result of applying the specified function to the vector is not of logical type")
    }
    
    if (invert)
        matches <- !matches
    if (na.rm)
        matches <- matches & !is.na(x)
    
    return (x[matches])
}
