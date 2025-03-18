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
        colWidths <- pmax(nameLengths + 1, contentLengths + 2)
        width <- colWidths[1]
        for (i in seq_along(groups)[-1])
        {
            
        }
    }
    
    cat("For information on a particular script, run \"tractor -o <script>\"\n\n", file=stderr())
}
