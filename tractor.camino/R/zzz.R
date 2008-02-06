.First.lib <- function (libname, pkgname)
{
    pos <- match(c("package:tractor.camino",".GlobalEnv"), search())
    pos <- pos[!is.na(pos)][1]
    
    tempCamino <- list(
        typeNames=c("byte",     "char",     "short",    "int",      "long",     "float",    "double"),
        typesR=c(   "integer",  "integer",  "integer",  "integer",  "integer",  "double",   "double"),
        sizes=c(     1,          1,          2,          4,          8,          4,          8),
        isSigned=c(  TRUE,       FALSE,      TRUE,       TRUE,       TRUE,       TRUE,       TRUE))
    
    assign(".Camino", tempCamino, pos=pos)
}
