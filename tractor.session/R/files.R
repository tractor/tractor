registerPathHandler("^(.+)@(\\w+)(/(\\w+))?", function(path) {
    # The match has to have been done just before calling this function (although this is not thread-safe)
    groups <- groups(ore.lastmatch())
    if (length(groups) == 2)
        newSessionFromDirectory(groups[1])$getImageFileNameByType(groups[2])
    else if (length(groups) == 3)
        newSessionFromDirectory(groups[1])$getImageFileNameByType(groups[3], groups[2])
    else
        return(NULL)
})
