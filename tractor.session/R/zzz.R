.First.lib <- function (libname, pkgname)
{
    pos <- match(c("package:tractor.session",".GlobalEnv"), search())
    pos <- pos[!is.na(pos)][1]
    
    tempCamino <- list(
        typeNames=c("byte",     "char",     "short",    "int",      "long",     "float",    "double"),
        typesR=c(   "integer",  "integer",  "integer",  "integer",  "integer",  "double",   "double"),
        sizes=c(     1,          1,          2,          4,          8,          4,          8),
        isSigned=c(  TRUE,       FALSE,      TRUE,       TRUE,       TRUE,       TRUE,       TRUE))
    
    assign(".Camino", tempCamino, pos=pos)
    
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (imageFileExists(file.path(tractorHome, "share", "mni", "brain")))
        assign(".StandardBrainPath", file.path(tractorHome,"share","mni"), pos=pos)
    else
        assign(".StandardBrainPath", NULL, pos=pos)
}
