"%~%" <- function (X, Y)
{
    if (!is.character(X) || !is.character(Y) || length(Y) != 1)
        report(OL$Error, "Parameters for matching on a regular expression are not valid")
    
    matchLocs <- regexpr(Y, X, perl=TRUE)
    return (!(matchLocs == -1))
}
