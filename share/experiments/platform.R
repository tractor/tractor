#@desc List information about the platform upon which TractoR is being run, including the operating system and versions of R, FSL and ImageMagick installed.

runExperiment <- function ()
{
    setOutputLevel(OL$Info, FALSE)
    
    sysInfo <- Sys.info()
    if (!is.null(sysInfo))
    {
        output(OL$Info, "Machine:             ", sysInfo["machine"])
        output(OL$Info, "OS name:             ", sysInfo["sysname"])
        output(OL$Info, "OS release:          ", sysInfo["release"])
    }
    
    tractorVersion <- readLines(file.path(Sys.getenv("TRACTOR_HOME"), "VERSION"))
    output(OL$Info, "TractoR version:     ", tractorVersion[1])
    
    rBuild <- R.Version()
    output(OL$Info, "R version:           ", rBuild$major, ".", rBuild$minor)
    output(OL$Info, "R build platform:    ", rBuild$platform)
    
    fslVersionFile <- file.path(Sys.getenv("FSLDIR"), "etc", "fslversion")
    if (file.exists(fslVersionFile))
        output(OL$Info, "FSL version:         ", readLines(fslVersionFile)[1])
    else
        output(OL$Info, "FSL version:         N/A (not found)")
    
    convertLoc <- locateExecutable("convert", errorIfMissing=FALSE)
    if (is.null(convertLoc))
        output(OL$Info, "ImageMagick version: N/A (not found)")
    else
    {
        versionString <- execute("convert", "-version", intern=TRUE)[1]
        output(OL$Info, "ImageMagick version: ", sub("^.+ImageMagick ([0-9.-]+) .+$","\\1",versionString))
    }
}
