#@desc List information about the platform upon which TractoR is being run, including the operating system and versions of R, FSL and ImageMagick installed.

runExperiment <- function ()
{
    sysInfo <- Sys.info()
    if (!is.null(sysInfo))
    {
        report(OL$Info, "Machine:             ", sysInfo["machine"])
        report(OL$Info, "OS name:             ", sysInfo["sysname"])
        report(OL$Info, "OS release:          ", sysInfo["release"])
    }
    
    tractorVersion <- readLines(file.path(Sys.getenv("TRACTOR_HOME"), "VERSION"))
    report(OL$Info, "TractoR version:     ", tractorVersion[1])
    
    rBuild <- R.Version()
    report(OL$Info, "R version:           ", rBuild$major, ".", rBuild$minor)
    report(OL$Info, "R build platform:    ", rBuild$platform)
    
    fslVersionFile <- file.path(Sys.getenv("FSLDIR"), "etc", "fslversion")
    if (file.exists(fslVersionFile))
        report(OL$Info, "FSL version:         ", readLines(fslVersionFile)[1])
    else
        report(OL$Info, "FSL version:         N/A (not found)")
    
    convertLoc <- locateExecutable("convert", errorIfMissing=FALSE)
    if (is.null(convertLoc))
        report(OL$Info, "ImageMagick version: N/A (not found)")
    else
    {
        versionString <- execute("convert", "-version", intern=TRUE)[1]
        report(OL$Info, "ImageMagick version: ", sub("^.+ImageMagick ([0-9.-]+) .+$","\\1",versionString))
    }
}
