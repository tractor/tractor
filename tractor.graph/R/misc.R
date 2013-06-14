printSparse <- function (x, threshold = 0, ignoreSign = TRUE, zeroChar = "", digits = 3, ...)
{
    if (ignoreSign)
        toRemove <- abs(x) < threshold
    else
        toRemove <- x < threshold
    
    strings <- format(replace(x,toRemove,0), digits=digits, zero.print=zeroChar, ...)
    print(strings, quote=FALSE)
    
    invisible(NULL)
}
