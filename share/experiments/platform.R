#@desc List information about the platform upon which TractoR is being run, including the operating system and versions of R and FSL installed.
#@nohistory TRUE

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
    
    gitAvailable <- !is.null(locateExecutable("git", errorIfMissing=FALSE))
    gitDirectory <- file.path(Sys.getenv("TRACTOR_HOME"), ".git")
    if (gitAvailable && file.exists(gitDirectory))
    {
        gitRepoVersion <- execute("git", es("--git-dir=\"#{gitDirectory}\" describe"), intern=TRUE)
        tractorVersion <- ore.subst("^v", "", gitRepoVersion)
    }
    else
        tractorVersion <- readLines(file.path(Sys.getenv("TRACTOR_HOME"), "VERSION"))
    
    rBuild <- R.Version()
    labels <- c(labels, "TractoR home directory", "TractoR version", "R version", "R build platform", "R package library")
    values <- c(values, Sys.getenv("TRACTOR_HOME"), tractorVersion[1], paste(rBuild$major,rBuild$minor,sep="."), rBuild$platform, .libPaths()[1])
    
    labels <- c(labels, "FSL version")
    
    fslVersionFile <- file.path(Sys.getenv("FSLDIR"), "etc", "fslversion")
    if (file.exists(fslVersionFile))
        values <- c(values, readLines(fslVersionFile)[1])
    else
        values <- c(values, "N/A (not found)")
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    printLabelledValues(labels, values)
}
