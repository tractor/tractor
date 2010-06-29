#@desc Collate partial data sets created by "pnt-data" into a complete data set. Usually this is done by "pnt-data" itself, but occasionally it may be necessary to collate data manually. This script is also used by "pnt-data-sge".
#@args [data files or file stem(s)]

suppressPackageStartupMessages(require(tractor.session))
suppressPackageStartupMessages(require(tractor.nt))

runExperiment <- function ()
{
    mergeDataTables <- function (table1, table2, allowIdClashes = FALSE)
    {
        if (!is.null(table1) && is.null(table1$subject))
            table1$subject <- rep(1,nrow(table1))
        if (!is.null(table2) && is.null(table2$subject))
            table2$subject <- rep(1,nrow(table2))
        
        if (!is.null(table1) && !is.null(table2) && !allowIdClashes)
        {
            subjects1 <- unique(table1$subject)
            subjects2 <- unique(table2$subject)
            
            if (!all(is.na(match(subjects1, subjects2))))
            {
                subjectLocs2 <- match(table2$subject, subjects2)
                subjects1 <- suppressWarnings(as.numeric(subjects1))
                subjects2 <- suppressWarnings(as.numeric(subjects2))

                if (any(is.na(subjects1)) || any(is.na(subjects2)))
                    output(OL$Error, "Subject fields in both data tables must be numeric")
                
                newSubjects2 <- subjects2 + max(subjects1)
                table2$subject <- newSubjects2[subjectLocs2]
            }
        }
        
        finalTable <- rbind(table1, table2)
        if (!is.null(finalTable))
            rownames(finalTable) <- as.character(1:nrow(finalTable))
        
        return (finalTable)
    }
    
    tractName <- getWithDefault("TractName", NULL, "character", errorIfMissing=TRUE)
    searchWidth <- getWithDefault("SearchWidth", NULL, "numeric")
    datasetName <- getWithDefault("DatasetName", "data")
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    
    if (nArguments() > 0)
        allNames <- Arguments
    else
        allNames <- datasetName
    
    blankRow <- createDataTableForSplines(list(NA), reference$getTract(), reference$getTractOptions()$pointType, 1)
    
    allData <- NULL
    for (name in allNames)
    {
        if (file.exists(ensureFileSuffix(name, "txt")))
            data <- read.table(ensureFileSuffix(name, "txt"))
        else
        {
            dirName <- dirname(expandFileName(name))
            baseName <- basename(expandFileName(name))
            subNames <- list.files(dirName, paste("^",baseName,".*\\.txt$",sep=""))
            
            completeSessions <- list()
            partialSessions <- list()
            for (subName in subNames)
            {
                if (subName %~% "_session\\d+\\.txt$")
                {
                    currentCompleteSession <- sub(".+_session(\\d+)\\.txt", "\\1", subName, perl=TRUE)
                    completeSessions[[currentCompleteSession]] <- subName
                }
                else if (subName %~% "_session\\d+_seed\\d+\\.txt$")
                {
                    currentPartialSession <- sub(".+_session(\\d+)_.+", "\\1", subName, perl=TRUE)
                    currentSeed <- sub(".+_seed(\\d+)\\.txt", "\\1", subName, perl=TRUE)
                    partialSessions[[currentPartialSession]][[currentSeed]] <- subName
                }
            }
            
            if (is.null(searchWidth) && length(partialSessions) > 0)
            {
                highestSeed <- -Inf
                for (s in names(partialSessions))
                    highestSeed <- max(highestSeed, as.numeric(names(partialSessions[[s]])))
                searchWidth <- ceiling(highestSeed^(1/3))
                output(OL$Info, "Guessing search width at ", searchWidth)
            }
            maxSeeds <- round(searchWidth^3)
            
            allSessionNumbers <- unique(as.numeric(c(names(completeSessions), names(partialSessions))))
            
            data <- NULL
            for (sessionNumber in sort(allSessionNumbers))
            {
                s <- as.character(sessionNumber)
                if (s %in% names(completeSessions))
                {
                    output(OL$Verbose, "Reading data for session ", s, " (single file)")
                    subData <- read.table(file.path(dirName, completeSessions[[s]]))
                    
                    if (s %in% names(partialSessions))
                        output(OL$Warning, "Data for session ", s, " was found in both single and multiple file forms; the latter will be ignored")
                }
                else
                {
                    output(OL$Verbose, "Reading data for session ", s, " (multiple files)")
                    subData <- NULL
                    for (seedNumber in 1:maxSeeds)
                    {
                        d <- as.character(seedNumber)
                        if (d %in% names(partialSessions[[s]]))
                            subsubData <- read.table(file.path(dirName, partialSessions[[s]][[d]]))
                        else
                            subsubData <- blankRow
                        subData <- mergeDataTables(subData, subsubData, allowIdClashes=TRUE)
                    }
                }
                
                if (length(unique(subData$subject)) > 1)
                    output(OL$Warning, "Data for session ", s, " contains more than one session identifier")
                if (as.character(subData$subject[1]) != s)
                    output(OL$Warning, "The stored identifier for session ", s, ", ", subData$subject[1], ", does not match the file name")
                
                data <- mergeDataTables(data, subData, allowIdClashes=FALSE)
            }
        }
        
        allData <- mergeDataTables(allData, data, allowIdClashes=FALSE)
    }
    
    output(OL$Info, "Writing out complete data table")
    write.table(allData, file=ensureFileSuffix(datasetName,"txt"))
}
