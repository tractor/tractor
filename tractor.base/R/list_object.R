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
