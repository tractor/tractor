#@desc List information about the platform upon which TractoR is being run, including the operating system and versions of R, FSL and ImageMagick installed.

runExperiment <- function ()
{
    labels <- character(0)
    values <- character(0)
    
    sysInfo <- Sys.info()
    if (!is.null(sysInfo))
    {
        labels <- c(labels, "Machine", "OS name", "OS release")
        values <- c(values, sysInfo["machine"], sysInfo["sysname"], sysInfo["release"])
    }
    
    tractorVersion <- readLines(file.path(Sys.getenv("TRACTOR_HOME"), "VERSION"))
    rBuild <- R.Version()
    labels <- c(labels, "TractoR home directory", "TractoR version", "R version", "R build platform")
    values <- c(values, Sys.getenv("TRACTOR_HOME"), tractorVersion[1], paste(rBuild$major,rBuild$minor,sep="."), rBuild$platform)
    
    labels <- c(labels, "FSL version", "ImageMagick version")
    
    fslVersionFile <- file.path(Sys.getenv("FSLDIR"), "etc", "fslversion")
    if (file.exists(fslVersionFile))
        values <- c(values, readLines(fslVersionFile)[1])
    else
        values <- c(values, "N/A (not found)")
    
    convertLoc <- locateExecutable("convert", errorIfMissing=FALSE)
    if (is.null(convertLoc))
        values <- c(values, "N/A (not found)")
    else
    {
        versionString <- execute("convert", "-version", intern=TRUE)[1]
        values <- c(values, sub("^.+ImageMagick ([0-9.-]+) .+$","\\1",versionString))
    }
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    printLabelledValues(labels, values)
}
