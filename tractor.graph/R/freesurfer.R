createCorticalThicknessTableForSessions <- function (sessionList, atlasName = "aparc")
{
    if (is.list(sessionList))
    {
        if (length(sessionList) == 0)
            report(OL$Error, "Specified session list is empty")
        else if (!is(sessionList[[1]], "MriSession"))
            report(OL$Error, "The specified list does not contain MriSession objects")
    }
    else if (is(sessionList, "MriSession"))
        sessionList <- list(sessionList)
    else
        report(OL$Error, "Sessions must be specified in a list")
    
    fullData <- NULL
    
    for (i in seq_along(sessionList))
    {
        session <- newSessionFromDirectory(sessionList[i])
        statsDirectory <- file.path(session$getDirectory("root"), "freesurfer", "stats")
        
        if (!file.exists(statsDirectory))
        {
            report(OL$Warning, "Freesurfer stats directory does not exist for session ", i)
            next
        }
        
        for (j in seq_along(.FreesurferHemispheres$prefix))
        {
            fileName <- ensureFileSuffix(file.path(statsDirectory, paste(.FreesurferHemispheres$prefix[j],atlasName,sep="")), "stats")
            if (file.exists(fileName))
            {
                lines <- readLines(fileName)
                connection <- textConnection(lines)
                stats <- read.table(connection)
                close(connection)
                
                headerLine <- grep("^#\\s*ColHeaders", lines, perl=TRUE, value=TRUE)
                colnames(stats) <- unlist(strsplit(sub("^#\\s*ColHeaders\\s*","",headerLine,perl=TRUE), "\\s+", perl=TRUE))
                
                data <- data.frame(Subject=i, Structure=paste(names(.FreesurferRegionNameMapping[[atlasName]]),.FreesurferHemispheres$suffix[j],sep=""), CorticalThickness=NA)
                for (k in seq_along(.FreesurferRegionNameMapping[[atlasName]]))
                {
                    indices <- match(.FreesurferRegionNameMapping[[atlasName]][[k]], stats[["StructName"]])
                    if (any(is.na(indices)))
                        report(OL$Error, "One or more expected atlas labels is missing for session ", i)
                    data$CorticalThickness[k] <- weighted.mean(stats[["ThickAvg"]][indices], stats[["SurfArea"]][indices])
                }
                
                fullData <- rbind(fullData, data)
            }
        }
    }
    
    reformattedData <- t(matrix(fullData$CorticalThickness, ncol=length(sessionList)))
    dimnames(reformattedData) <- list(Structure=fullData$Structure[fullData$Subject==1], Subject=as.character(seq_along(sessionList)))
    
    return (reformattedData)
}
