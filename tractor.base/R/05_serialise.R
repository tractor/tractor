#' The TractorObject class
#' 
#' This reference class extends the standard \code{\linkS4class{envRefClass}}
#' class, adding methods for finding all of the field or methods available for
#' an object. There is also a method for summarising key elements of the object
#' as a named character vector, which can be suitable overridden by inheriting
#' classes. The \code{show} method prints this summary as a labelled list.
#' 
#' @export
TractorObject <- setRefClass("TractorObject", methods=list(
    fields = function ()
    {
        "Retrieve a list of all field names"
        allFieldNames <- names(.self$getRefClass()$fields())
        if (is.null(allFieldNames))
            return (NULL)
        else
            return (allFieldNames[!(allFieldNames %~% "\\.$")])
    },
    
    methods = function ()
    {
        "Retrieve a list of all method names"
        return (.self$getRefClass()$methods())
    },
    
    summarise = function ()
    {
        "Summarise key aspects of the object"
        return (c("Object class"=class(.self)[1]))
    }
))

setMethod("show", "TractorObject", function (object)
{
    summaryList <- object$summarise()
    if (is.list(summaryList) && all(c("labels","values") %in% names(summaryList)))
        printLabelledValues(summaryList$labels, summaryList$values)
    else if (!is.null(names(summaryList)))
        printLabelledValues(names(summaryList), as.character(summaryList))
})

#' Lists of serialisable objects
#' 
#' This function creates objects of S3 class \code{"loso"}, which are standard
#' lists with this class attribute set. These objects can be used within
#' \code{\linkS4class{SerialisableObject}} objects for fields that contain
#' multiple objects which may themselves be serialisable. Otherwise their
#' functionality is the same as a normal list.
#' 
#' @param ... List elements.
#' @param count If only one element is provided, an optional integer specifying
#'   the number of times to repeat the element. This allows lists of a fixed
#'   size to be created using a default value.
#' @return A list of class \code{"loso"}.
#' @note TractoR's serialisable objects are reference classes, and so naive use
#'   of \code{\link{rep}} to create a list of object copies will have reference
#'   semantics whereby each object refers to the same set of fields, which is
#'   rarely the intention. This function explicitly creates a deep copy for
#'   each replicate to avoid this.
#' @author Jon Clayden
#' @seealso \code{\linkS4class{SerialisableObject}}, \code{\link{save}},
#' \code{\link{load}}, \code{\link{writeImageFile}}.
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
loso <- function (..., count = NA)
{
    value <- list(...)
    if (length(value) == 1L && !is.na(count))
    {
        value <- rep(value, count)
        # Reference objects need to be explicitly deep-copied
        for (i in seq_len(count-1))
            value[[i+1]] <- value[[i+1]]$copy()
    }
    return (structure(value, class="loso"))
}

setOldClass("loso")

#' The SerialisableObject class
#' 
#' This reference class extends \code{\linkS4class{TractorObject}} by adding a
#' function for simple serialisation of the data fields of an object, either to
#' a list or a file. This is intended to be used for classes whose state can
#' meaningfully be restored from a list of standard R objects (not including
#' transient C/C++ pointers, for example). A serialised object may be
#' deserialised using the \code{\link{deserialiseReferenceObject}} function.
#' 
#' @seealso \code{\link{save}}
#' @export
SerialisableObject <- setRefClass("SerialisableObject", contains="TractorObject", methods=list(
    serialise = function (file = NULL)
    {
        "Serialise the object to a list or file"
        serialiseReferenceObject(.self, file)
    }
))

.NilObject <- SerialisableObject$new()

#' The nil object
#' 
#' The nil object is an empty object of class \code{\link{SerialisableObject}}.
#' It can be used as a placeholder where such an object of this class, or one
#' of its subclasses, is required. It serialises to the empty list.
#' 
#' @param object Any object.
#' @return \code{nilObject} returns the nil object. \code{is.nilObject} returns
#'   \code{TRUE} if its argument is identical to the nil object, or if it is
#'   equivalent in the sense of serialising to an identical result.
#' 
#' @author Jon Clayden
#' @seealso \code{\link{SerialisableObject}}
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
nilObject <- function ()
{
    return (.NilObject)
}

#' @rdname nilObject
#' @export
is.nilObject <- function (object)
{
    if (identical(object, .NilObject))
        return (TRUE)
    else if (identical(object$serialise(), .NilObject$serialise()))
        return (TRUE)
    else
        return (FALSE)
}

#' Reference object serialisation and deserialisation
#' 
#' Rather than using R's \code{\link{save}} and \code{\link{load}} functions
#' directly for reference objects, TractoR uses the
#' \code{\linkS4class{SerialisableObject}} class and these functions to save
#' and load objects. The main difference is that this approach stores only the
#' data in the object, and not the functions which operate on them. This helps
#' backward compatibility when new member functions are added.
#' 
#' The \code{serialiseReferenceObject} function, or the \code{serialise} member
#' function of the \code{\link{SerialisableObject}} class, can be used to
#' create and/or \code{\link{save}} a version of an object which contains a
#' hierarchical representation of the data embedded in it. These serialised
#' objects are standard R lists, with an \code{"originalClass"} attribute
#' describing the class of the original object. The
#' \code{deserialiseReferenceObject} function can be used to deserialise them.
#' Custom deserialisers can be specified using \code{registerDeserialiser},
#' typically for legacy classes.
#' 
#' Note that this should generally NOT be used as the primary mechanism for
#' saving and loading \code{\link{MriImage}} objects. Saving to standard
#' NIfTI/Analyze format is usually preferable, and can be done using
#' \code{\link{writeImageFile}}.
#' 
#' @param object For \code{serialiseReferenceObject}, any object, but only
#'   those inheriting from \code{\linkS4class{SerialisableObject}} and
#'   \code{\link{loso}} objects are treated specially. For other functions, an
#'   object in (raw) serialised form. See Details.
#' @param expectedClass A class name which the object is expected to inherit.
#'   Any class is acceptable if this parameter is \code{NULL}.
#' @param file A file name to deserialise from.
#' @param raw If \code{TRUE}, the raw serialised object is returned; otherwise
#'   the object is converted back to its original class.
#' @param className A string naming a class to be handled by the specified
#'   deserialiser.
#' @param deserialiser A function taking as its argument a list of serialised
#'   fields, and returning a suitable deserialised object.
#' @return \code{isDeserialisable} returns \code{TRUE} if the \code{object} is
#'   deserialisable and (if specified) inherits from the required class,
#'   otherwise \code{FALSE}. If its argument is serialisable then
#'   \code{serialiseReferenceObject} returns the serialised version, invisibly;
#'   otherwise it returns the unmodified object, again invisibly.
#'   \code{deserialiseReferenceObject} returns a raw or reconstituted object
#'   after deserialisation.
#' 
#' @author Jon Clayden
#' @seealso \code{\linkS4class{SerialisableObject}}, \code{\link{save}},
#' \code{\link{load}}, \code{\link{loso}}, \code{\link{writeImageFile}}.
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @aliases serialisation
#' @rdname serialisation
#' @export
isDeserialisable <- function (object, expectedClass = NULL)
{
    if (is.null(object) || is.null(attr(object,"originalClass")))
        return (FALSE)
    else if (!is.null(expectedClass) && !(expectedClass %in% attr(object,"originalClass")))
        return (FALSE)
    else
        return (TRUE)
}

#' @rdname serialisation
#' @export
serialiseReferenceObject <- function (object, file = NULL)
{
    originalClass <- class(object)
    originalPackage <- attr(originalClass, "package")
    attributes(originalClass) <- NULL
    
    if (is(object, "SerialisableObject"))
    {
        # Fields with names ending in "." will not be returned by the fields() method, and therefore not be serialised
        serialisedObject <- sapply(object$fields(), function (name) {
            field <- object$field(name)
            serialiseReferenceObject(field)
        }, simplify=FALSE, USE.NAMES=TRUE)
        serialisedObject <- structure(serialisedObject, originalClass=originalClass, originalPackage=originalPackage)
    }
    else if (is(object, "loso"))
        serialisedObject <- structure(lapply(object,serialiseReferenceObject), originalClass="loso")
    else
        serialisedObject <- object
    
    if (!is.null(file))
        save(serialisedObject, file=ensureFileSuffix(file,"Rdata"))
    
    invisible(serialisedObject)
}

#' @rdname serialisation
#' @export
deserialiseReferenceObject <- function (file = NULL, object = NULL, raw = FALSE)
{
    if (is.null(object))
    {
        if (is.null(file))
            report(OL$Error, "Either a file or raw deserialised object must be specified")
        object <- get(load(ensureFileSuffix(file,"Rdata")))
    }
    
    assert(isDeserialisable(object), "The specified object or file is not deserialisable")
    
    if (raw)
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
    if (className %in% names(.Workspace$deserialisers))
        finalObject <- .Workspace$deserialisers[[className]](fields)
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

#' @rdname serialisation
#' @export
registerDeserialiser <- function (className, deserialiser)
{
    if (!is.character(className) || length(className) != 1)
        report(OL$Error, "Class name should be specified as a character string")
    
    deserialiser <- match.fun(deserialiser)
    .Workspace$deserialisers[[className]] <- deserialiser
    
    invisible(NULL)
}

#' Optional types
#' 
#' This function declares an optional type, a union of the named class (which
#' must already be defined) and \code{NULL}. It is meant for use in packages.
#' 
#' @param className A string naming an existing (S4) class.
#' @return The name of the union class (invisibly), which will be the original
#'   class name with "Optional" prepended to it.
#' @author Jon Clayden
#' @seealso \code{\link[methods]{setClassUnion}}
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
Optional <- function (className)
{
    unionName <- paste0("Optional", className)
    if (!isClassUnion(unionName))
        setClassUnion(unionName, c(className,"NULL"), where=topenv(parent.frame()))
    invisible(unionName)
}
