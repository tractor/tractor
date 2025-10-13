#@desc Check for a newer version of TractoR, and optionally update the installation. With Reinstall:true, the existing installation will be overwritten even if the latest version is not newer.
#@group Utilities
#@interactive TRUE
#@nohistory TRUE

runExperiment <- function ()
{
    reinstall <- getConfigVariable("Reinstall", FALSE)
    
    connection <- url("http://www.tractor-mri.org.uk/latest.txt", "r")
    latest <- gsub("[^0-9.]+", "", readLines(connection)[1], perl=TRUE)
    close(connection)
    
    fileName <- file.path(Sys.getenv("TRACTOR_HOME"), "VERSION")
    current <- gsub("[^0-9.]+", "", readLines(fileName)[1], perl=TRUE)
    
    latestVersion <- as.integer(unlist(strsplit(latest, ".", fixed=TRUE)))
    latestVersion <- sum(latestVersion * c(10000, 100, 1))
    currentVersion <- as.integer(unlist(strsplit(current, ".", fixed=TRUE)))
    currentVersion <- sum(currentVersion * c(10000, 100, 1))
    
    if (getOutputLevel() > OL$Info)
        setOutputLevel(OL$Info)
    
    report(OL$Info, "Installed version is v", current)
    report(OL$Info, "Latest version is v", latest)
    
    if (reinstall || latestVersion > currentVersion)
    {
        if (ask("Would you like to update your installation? [yn]", valid=c("y","n")) == "y")
        {
            targetFileName <- ensureFileSuffix(tempfile(), "tar.gz")
            targetDir <- file.path(tempdir(), "download")
            
            report(OL$Info, "Downloading latest TractoR package...")
            download.file("http://www.tractor-mri.org.uk/tractor.tar.gz", targetFileName, method="internal", quiet=FALSE, mode="wb")
            
            report(OL$Info, "Unpacking archive to ", targetDir)
            returnCode <- untar(targetFileName, exdir=targetDir, compressed="gzip")
            
            if (returnCode == 0)
            {
                report(OL$Info, "Moving unpacked directory to ", Sys.getenv("TRACTOR_HOME"))
                success <- copyDirectory(file.path(targetDir,"tractor"), Sys.getenv("TRACTOR_HOME"), deleteOriginal=TRUE, overwrite=TRUE)
                if (success)
                    report(OL$Info, "Update complete - you may wish to run \"make install\" or \"make install-local\" from the TractoR home directory now")
                else
                    report(OL$Error, "Moving the directory into place failed")
            }
            else
                report(OL$Error, "The unpacking operation did not report success")
        }
    }
    
    invisible(NULL)
}

