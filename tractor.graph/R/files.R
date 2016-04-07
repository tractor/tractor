readGraphFile <- function (fileName, fileType = NULL)
{
    if (!is.null(fileType))
        fileType <- match.arg(fileType, c("binary","csv"))
    else if (file.exists(ensureFileSuffix(fileName, "Rdata")))
        fileType <- "binary"
    else if (file.exists(ensureFileSuffix(fileName, "csv")))
        fileType <- "csv"
    else
        report(OL$Error, "No suitable graph file found with name \"#{fileName}\"")
    
    if (fileType == "binary")
    {
        object <- deserialiseReferenceObject(fileName)
        if (!is(object, "Graph"))
            report(OL$Error, "Deserialised object is not a graph")
        else
            return (object)
    }
    else if (fileType == "csv")
    {
        fileName <- ensureFileSuffix(fileName, "csv")
        lines <- readLines(fileName)
        
        connection <- textConnection(lines[!(lines %~% "^\\s*#")])
        graph <- asGraph(as.matrix(read.csv(connection, header=FALSE)))
        close(connection)
        
        matches <- ore.search("^\\s*#\\s*([A-Za-z]+):\\s*(.+)$", lines, simplify=FALSE)
        if (!is.null(matches))
        {
            groups <- groups(matches, simplify=TRUE)
            attribs <- lapply(groups[,2,], function(x) {
                x <- ore.split(ore(",",syntax="fixed"), x)
                if (!any(is.na(suppressWarnings(as(x, "numeric")))))
                    x <- as.numeric(x)
                return (x)
            })
            names(attribs) <- groups[,1,]
            do.call(graph$setVertexAttributes, attribs)
        }
        
        return (graph)
    }
}

writeGraphFile <- function (graph, fileName, fileType = c("binary","csv"))
{
    fileType <- match.arg(fileType)
    
    if (fileType == "binary")
        graph$serialise(fileName)
    else if (fileType == "csv")
    {
        fileName <- ensureFileSuffix(fileName, "csv")
        connection <- file(fileName, "w")
        
        attribStrings <- sapply(graph$getVertexAttributes(), function(attrib) paste0(": ", implode(attrib,",")))
        if (length(attribStrings) > 0)
        {
            attribStrings <- paste0("# ", names(graph$getVertexAttributes()), attribStrings)
            writeLines(attribStrings, connection)
        }
    
        write.table(graph$getAssociationMatrix(), connection, sep=",", row.names=FALSE, col.names=FALSE)
    
        close(connection)
    }
}
