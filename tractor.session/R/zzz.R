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
    
    fslHome <- Sys.getenv("FSLDIR")
    if (is.character(fslHome) && nchar(fslHome) > 0)
    {
        fslHome <- expandFileName(fslHome)
        candidatePaths <- c(file.path(fslHome,"data","standard","tissuepriors"), file.path(fslHome,"etc","standard"))
        for (path in candidatePaths)
        {
            if (imageFileExists(file.path(path, "avg152T1_brain")))
            {
                assign(".StandardBrainPath", path, pos=pos)
                break
            }
        }
    }
    
    if (!exists(".StandardBrainPath"))
        assign(".StandardBrainPath", NULL, pos=pos)
    
    fslFileType <- toupper(Sys.getenv("FSLOUTPUTTYPE"))
    if (is.null(getOption("tractorFileType")) && isTRUE(fslFileType %in% .FileTypes$typeNames))
        options(tractorFileType=as.vector(fslFileType))
}
