.First.lib <- function (libname, pkgname)
{
    pos <- match(c("package:tractor.fsl",".GlobalEnv"), search())
    pos <- pos[!is.na(pos)][1]
    
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
    
    fslFileType <- Sys.getenv("FSLOUTPUTTYPE")
    if (fslFileType %in% .FileTypes$typeNames)
        options(tractorFileType=as.vector(fslFileType))
}
