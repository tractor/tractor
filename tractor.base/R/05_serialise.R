SerialisableObject <- setRefClass("SerialisableObject", methods=list(
    fields = function ()
    {
        allFieldNames <- names(.self$getRefClass()$fields())
        if (is.null(allFieldNames))
            return (NULL)
        else
            return (allFieldNames[allFieldNames %!~% "\\.$"])
    },
    
    methods = function () { return (.self$getRefClass()$methods()) },
    
    serialise = function (file = NULL)
    {
        originalClass <- class(.self)
        originalPackage <- attr(originalClass,"package")
        attributes(originalClass) <- NULL
        
        # Fields with names ending in "." will not be returned, and therefore not be serialised
        fields <- .self$fields()
        serialisedObject <- list()

        for (field in fields)
        {
            fieldValue <- get(field)

            if (is(fieldValue, "SerialisableObject"))
                serialisedObject <- c(serialisedObject, list(fieldValue$serialise(NULL)))
            else
                serialisedObject <- c(serialisedObject, list(fieldValue))
        }

        names(serialisedObject) <- fields
        attr(serialisedObject, "originalClass") <- originalClass
        attr(serialisedObject, "originalPackage") <- originalPackage

        if (!is.null(file))
            save(serialisedObject, file=ensureFileSuffix(file,"Rdata"))
        
        invisible(serialisedObject)
    }
))

setMethod("show", "SerialisableObject", function (object)
{
    if ("summarise" %in% object$methods())
    {
        summaryList <- object$summarise()
        if (is.list(summaryList) && all(c("labels","values") %in% names(summaryList)))
            printLabelledValues(summaryList$labels, summaryList$values)
        else if (!is.null(names(summaryList)))
            printLabelledValues(names(summaryList), as.character(summaryList))
    }
    else
        cat(paste("An object of class \"", class(object)[1], "\"\n", sep=""))
})

.NilObject <- SerialisableObject$new()

nilObject <- function ()
{
    return (.NilObject)
}

is.nilObject <- function (object)
{
    if (identical(object, .NilObject))
        return (TRUE)
    else if (identical(object$serialise(), .NilObject$serialise()))
        return (TRUE)
    else
        return (FALSE)
}

isDeserialisable <- function (object, expectedClass = NULL)
{
    if (is.null(object) || is.null(attr(object,"originalClass")))
        return (FALSE)
    else if (!is.null(expectedClass) && !(expectedClass %in% attr(object,"originalClass")))
        return (FALSE)
    else
        return (TRUE)
}

serialiseReferenceObject <- function (object, file = NULL)
{
    if (is(object, "SerialisableObject"))
        serialisedObject <- object$serialise()
    else if (is.list(object))
        serialisedObject <- lapply(object, serialiseReferenceObject)
    else
        report(OL$Error, "Object to serialise must be a list or a SerialisableObject")
    
    if (!is.null(file))
        save(serialisedObject, file=ensureFileSuffix(file,"Rdata"))
    
    invisible(serialisedObject)
}

deserialiseReferenceObject <- function (file = NULL, object = NULL, raw = FALSE)
{
    if (is.null(object))
    {
        if (is.null(file))
            report(OL$Error, "Either a file or raw deserialised object must be specified")
        object <- get(load(ensureFileSuffix(file,"Rdata")))
    }
    
    if (!isDeserialisable(object))
    {
        if (is.list(object))
            return (invisible(lapply(object, function(x) deserialiseReferenceObject(object=x,raw=raw))))
        else
            report(OL$Error, "The specified object or file is not deserialisable")
    }
    else if (raw)
        return (invisible(object))
    
    fields <- lapply(object, function (field) {
        if (isDeserialisable(field))
            return (deserialiseReferenceObject(object=field))
        else
            return (field)
    })
    names(fields) <- names(object)
    
    packageName <- attr(object, "originalPackage")
    if (!is.null(packageName) && !(paste("package",packageName,sep=":") %in% search()))
        require(packageName, character.only=TRUE, quietly=TRUE)
    
    className <- attr(object, "originalClass")
    if (className %in% names(.Deserialisers))
        finalObject <- .Deserialisers[[className]](fields)
    else
    {
        if (!is.null(packageName))
            class <- getRefClass(className, where=as.environment(paste("package",packageName,sep=":")))
        else
            class <- getRefClass(className)
        finalObject <- do.call(class$new, fields)
    }
    
    invisible(finalObject)
}

registerDeserialiser <- function (className, deserialiser)
{
    if (!is.character(className) || length(className) != 1)
        report(OL$Error, "Class name should be specified as a character string")
    
    deserialiser <- match.fun(deserialiser)
    .Deserialisers[[className]] <<- deserialiser
    
    invisible(NULL)
}
