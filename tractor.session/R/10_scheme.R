SimpleDiffusionScheme <- setRefClass("SimpleDiffusionScheme", contains="SerialisableObject", fields=list(bValues="numeric",gradientDirections="list"), methods=list(
    initialize = function (...)
    {
        params <- list(...)
        if (!all(c("bValues","gradientDirections") %in% names(params)))
            report(OL$Error, "Gradient directions and b-values must be specified")
        if (is.matrix(params$gradientDirections))
            params$gradientDirections <- list(params$gradientDirections)
        if (length(params$bValues) != length(params$gradientDirections))
            report(OL$Error, "An equal number of gradient subsets and b-values must be given")
        params$gradientDirections <- lapply(params$gradientDirections, validateGradientDirections)
        
        initFields(bValues=params$bValues, gradientDirections=params$gradientDirections)
    },
    
    expandComponents = function ()
    {
        returnValue <- list()
        returnValue$directions <- Reduce(cbind, .gradientDirections)
        returnValue$bValues <- rep(.bValues, .self$nDirections())
        return (returnValue)
    },
    
    getBValues = function () { return (bValues) },
    
    getGradientDirections = function () { return (gradientDirections) },
    
    nDirections = function () { return (sapply(gradientDirections, ncol)) },
    
    summarise = function ()
    {
        report(OL$Info, "Number of gradient directions: ", implode(nDirections(),", "))
        report(OL$Info, "Diffusion b-values           : ", implode(round(bValues),", "), " s/mm^2")
    }
))

validateGradientDirections <- function (directions)
{
    if (!is.matrix(directions))
        report(OL$Error, "Gradient directions must be specified in a matrix")
    else if (ncol(directions) != 3 && nrow(directions) != 3)
        report(OL$Error, "Gradient directions should be specified as a 3xN matrix")
    else if (ncol(directions) == 3 && nrow(directions) != 3)
    {
        report(OL$Info, "Transposing gradient direction matrix")
        directions <- t(directions)
    }
    
    # Normalise directions to unit length
    directions <- apply(directions, 2, function (x) {
        length <- vectorLength(x)
        return (x / ifelse(length==0,1,length))
    })
    
    return (directions)
}

newSimpleDiffusionSchemeWithDirections <- function (directions, bValues)
{
    if (is.list(directions))
        invisible(SimpleDiffusionScheme$new(bValues=bValues, gradientDirections=gradientDirections))
    else if (is.matrix(directions))
    {
        if (ncol(directions) == 3 && nrow(directions) != 3)
        {
            report(OL$Info, "Transposing gradient direction matrix")
            directions <- t(directions)
        }
        
        # Find groups with the same b-value
        bValueRunLengths <- rle(bValues)
        subsetBreaks <- cumsum(c(0, bValueRunLengths$lengths))
        gradientDirections <- lapply(seq_along(bValueRunLengths$lengths), function (i) directions[,(subsetBreaks[i]+1):subsetBreaks[i+1],drop=FALSE])
        
        invisible(SimpleDiffusionScheme$new(bValues=bValueRunLengths$values, gradientDirections=gradientDirections))
    }
    else
        report(OL$Error, "Gradient directions must be specified as a matrix or list of matrices")
}

newSimpleDiffusionSchemeFromSession <- function (session)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    directory <- session$getDirectory("fdt")
    bValues <- unlist(read.table(file.path(directory, "bvals")))
    directions <- as.matrix(read.table(file.path(directory, "bvecs")))
    names(bValues) <- NULL
    dimnames(directions) <- NULL
    
    scheme <- newSimpleDiffusionSchemeWithDirections(directions, bValues)
    invisible(scheme)
}

writeSimpleDiffusionSchemeForSession <- function (session, scheme)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!is(scheme, "SimpleDiffusionScheme"))
        report(OL$Error, "Specified scheme is not a SimpleDiffusionScheme object")
    
    fslDir <- session$getDirectory("fdt")
    caminoDir <- session$getDirectory("camino")
    
    components <- scheme$expandComponents()
    
    if (file.exists(fslDir))
    {
        write.table(matrix(components$bValues,nrow=1), file.path(fslDir,"bvals"), row.names=FALSE, col.names=FALSE)
        write.table(components$directions, file.path(fslDir,"bvecs"), row.names=FALSE, col.names=FALSE)
    }
    
    if (file.exists(caminoDir))
    {
        bvecMatrix <- cbind(t(components$directions), components$bValues)
        lines <- apply(format(bvecMatrix), 1, implode, sep=" ")
        lines <- c("VERSION: 2", lines)
        writeLines(lines, file.path(caminoDir,"sequence.scheme"))
    }
}
