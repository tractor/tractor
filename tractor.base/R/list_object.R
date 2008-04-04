isListObject <- function (object)
{
    return ("list.object" %in% class(object))
}

print.list.object <- function (x, ...)
{
    if (is.null(x$summarise))
        print(summary(x))
    else
        x$summarise()
}

inherit <- function (child, parent)
{
    if (!isListObject(child) || !isListObject(parent))
        output(OL$Error, "Parent and child must be list objects")
    
    composite <- c(child, parent)
    composite <- composite[!duplicated(names(composite))]
    
    childClass <- class(child)
    childClass <- childClass[!(childClass %in% c("list.object", "list"))]
    parentClass <- class(parent)
    parentClass <- parentClass[!(parentClass %in% c("list.object", "list"))]
    class(composite) <- unique(c(childClass, parentClass, "list.object", "list"))
    
    invisible(composite)
}

serialiseListObject <- function (object, file = NULL)
{
    if (!isListObject(object))
        output(OL$Error, "The specified object is not a list object")
    if (length(object) == 0)
    {
        output(OL$Warning, "The specified list object is empty")
        return (invisible(NULL))
    }
    if (class(object[[1]]) != "function")
        output(OL$Error, "The list object does not seem to contain a list of functions")
    
    envir <- environment(object[[1]])
    contents <- ls(envir, all.names=TRUE, pattern="^\\.")
    
    serialisedObject <- list()
    
    for (itemName in contents)
    {
        item <- get(itemName, envir=envir)
        
        if (isListObject(item))
            serialisedObject <- c(serialisedObject, list(serialiseListObject(item,NULL)))
        else
            serialisedObject <- c(serialisedObject, list(item))
    }
    
    names(serialisedObject) <- sub("^\\.", "", contents)
    attr(serialisedObject, "originalClass") <- class(object)
    
    if (is.null(file))
        invisible (serialisedObject)
    else
        save(serialisedObject, file=file)
}

deserialiseListObject <- function (file = NULL, object = NULL, constructor = NULL, defaults = list(), raw = FALSE)
{
    if (is.null(object))
    {
        if (is.null(file))
            output(OL$Error, "Either a file or raw deserialised object must be specified")
        object <- get(load(file))
    }
    
    if (!isDeserialisable(object))
        output(OL$Info, "The specified object or file is not deserialisable")
    
    if (raw)
        return (invisible(object))
    else if (is.null(constructor))
        output(OL$Error, "Constructor must be specified")
    
    constructor <- match.fun(constructor)
    params <- sub("^\\.", "", names(formals(constructor)))
    
    args <- list()
    for (param in params)
    {
        if (param %in% names(object))
            args <- c(args, list(object[[param]]))
        else if (param %in% names(defaults))
            args <- c(args, list(defaults[[param]]))
        else
            output(OL$Error, "Required parameter \"", param, "\" not found in the deserialised object")
    }
    names(args) <- paste(".", params, sep="")
    
    finalObject <- do.call(constructor, args)
    invisible (finalObject)
}

isDeserialisable <- function (object, expectedClass = "list.object")
{
    if (is.null(object) || is.null(attr(object,"originalClass")))
        return (FALSE)
    else if (!(expectedClass %in% attr(object,"originalClass")))
        return (FALSE)
    else
        return (TRUE)
}
