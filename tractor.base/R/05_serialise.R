SerialisableObject <- setRefClass("SerialisableObject", methods=list(
    fields = function ()
    {
        allFieldNames <- names(.self$getRefClass()$fields())
        return (allFieldNames[allFieldNames %!~% "\\.$"])
    },
    
    methods = function () { return (.self$getRefClass()$methods()) },
    
    serialise = function (file = NULL)
    {
        originalClass <- class(.self)
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

        if (is.null(file))
            invisible (serialisedObject)
        else
            save(serialisedObject, file=file)
    }
))

setMethod("show", "SerialisableObject", function (object)
{
    if ("summarise" %in% object$methods())
    {
        summaryList <- object$summarise()
        if (is.list(summaryList) && all(c("labels","values") %in% names(summaryList)))
            printLabelledValues(summaryList$labels, summaryList$values)
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

deserialiseReferenceObject <- function (file = NULL, object = NULL, raw = FALSE)
{
    if (is.null(object))
    {
        if (is.null(file))
            report(OL$Error, "Either a file or raw deserialised object must be specified")
        object <- get(load(file))
    }
    
    if (!isDeserialisable(object))
        report(OL$Error, "The specified object or file is not deserialisable")
    else if (raw)
        return (invisible(object))
    
    fields <- lapply(object, function (field) {
        if (isDeserialisable(field))
            return (deserialiseReferenceObject(object=field))
        else
            return (field)
    })
    names(fields) <- names(object)
    
    class <- getRefClass(attr(object, "originalClass"))
    finalObject <- do.call(class$new, fields)
    invisible (finalObject)
}
