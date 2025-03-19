#@desc List all available TractoR experiment scripts, arranged into categories.
#@group Utilities
#@nohistory TRUE

library(tractor.base)

runExperiment <- function ()
{
    descriptions <- getConfigVariable("Descriptions", FALSE)
    
    expts <- scanExperiments()
    groups <- with(expts, tapply(name, group, "[", simplify=FALSE))
    unnamed <- which(names(groups) == "")
    if (length(unnamed) == 1L)
        groups <- c(groups[-unnamed], list("(Ungrouped)"=groups[[unnamed]]))
    
    groupNames <- names(groups)
    nameLengths <- nchar(groupNames)
    contentLengths <- sapply(groups, fx(max(nchar(x))))
    screenWidth <- getOption("width", 80L)
    
    if (descriptions)
    {
        contentWidth <- max(contentLengths)
        descriptionWidth <- screenWidth - contentWidth - 6L
        for (i in seq_along(groups))
        {
            padding <- contentWidth - nchar(groups[[i]]) + 2
            cat(colour(groupNames[i],"cyan","bold"), ":\n", sep="")
            for (j in seq_along(groups[[i]]))
            {
                name <- groups[[i]][j]
                description <- expts$description[expts$name==name]
                if (nchar(description) == 0L)
                    description <- "(No description)"
                else if (nchar(description) > descriptionWidth)
                    description <- paste0(substr(description, 1, descriptionWidth-3), "...")
                cat("  ", name, rep(" ",padding[j]), colour(description, "white", "italic"), "\n", sep="")
            }
            cat("\n")
        }
    }
    else
    {
        # The width required is the greater of the group name plus a colon and the script names plus left-padding
        colWidths <- pmax(nameLengths + 1, contentLengths + 2)
        width <- 0
        lines <- character(0L)
        for (i in seq_along(groups))
        {
            # Check if there is space for the next column (plus padding)
            # If not, write out and reset the buffer
            if (width + colWidths[i] + 2 > screenWidth)
            {
                cat(paste(lines, collapse="\n"), "\n\n", sep="")
                width <- 0
                lines <- character(0L)
            }
            
            # Check whether there are more or less lines than needed for this group and its header
            lineCountMismatch <- length(groups[[i]]) + 1 - length(lines)
            if (lineCountMismatch > 0)
                lines <- c(lines, rep(paste(rep(" ",width),collapse=""), lineCountMismatch))
            lines[1] <- paste0(lines[1], colour(groupNames[i],"cyan","bold"), ":  ", paste0(rep(" ", colWidths[i] - nameLengths[i] - 1),collapse=""))
            
            padding <- colWidths[i] - nchar(groups[[i]])
            for (j in seq_along(groups[[i]]))
                lines[j+1] <- paste0(lines[j+1], "  ", groups[[i]][j], paste0(rep(" ", padding[j]), collapse=""))
            
            if (lineCountMismatch < 0)
                lines[(length(lines)+lineCountMismatch+1):length(lines)] <- paste0(lines[(length(lines)+lineCountMismatch+1):length(lines)], paste0(rep(" ", colWidths[i]+2), collapse=""))
            
            width <- width + colWidths[i] + 2
        }
        
        if (length(lines) > 0)
            cat(paste(lines, collapse="\n"), "\n\n", sep="")
    }
    
    cat(colour("For information on a particular script, run \"tractor -o <script>\"", "yellow"), "\n", sep="")
}
