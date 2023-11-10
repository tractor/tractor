.onLoad <- function (libname, pkgname)
{
    tractorHome <- Sys.getenv("TRACTOR_HOME")
    if (imageFileExists(file.path(tractorHome, "share", "tractor", "mni", "brain")))
        .StandardBrainPath <<- file.path(tractorHome, "share", "tractor", "mni")
    
    if (is.null(getOption("tractorViewer")))
    {
        viewer <- tolower(Sys.getenv("TRACTOR_VIEWER"))
        if (isTRUE(viewer %in% .Viewers))
            options(tractorViewer=as.vector(viewer))
        else
            options(tractorViewer="tractor")
    }
    
    # Assume path separator (.Platform$file.sep) is "/"
    registerPathHandler("^(?<dir>[^@=\\s]+)?@((?<baresubdir>\\w+/)|(?<subdir>\\w+/)?(?<name>[\\w-]+)(?<index>%(\\d+))?(?<suffix>(\\.\\w+){0,2}))$", function(path, index=1L, defaultSessionPath=".") {
    #                     | optional session path (default ".")
    #                                         | bare subdirectory branch (should end with a slash)
    #                                                              | file branch (subdirectory optional)
        groups <- drop(groups(attr(path, "match")))
        groupsPresent <- !is.na(groups)
        
        # The string matches, so the name group must be present
        session <- attachMriSession(ifelse(groupsPresent["dir"], groups["dir"], defaultSessionPath))
        if (groupsPresent["index"])
            index <- as.integer(ore_subst("$%", "", groups["index"]))
        
        if (groupsPresent["baresubdir"])
            return (session$getDirectory(ore_subst("/$", "", groups["baresubdir"])))
        else
        {
            place <- NULL
            if (groupsPresent["subdir"])
                place <- ore_subst("/$", "", groups["subdir"])
            path <- session$getImageFileNameByType(groups["name"], place=place, index=index, fallback=TRUE)
            if (groupsPresent["suffix"])
                path <- paste0(path, groups["suffix"])
            return (path)
        }
    })
}
