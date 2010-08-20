.SimpleDiffusionScheme <- function (.bValues, .gradientDirections)
{
    if (is.matrix(.gradientDirections))
        .gradientDirections <- list(.gradientDirections)
    if (!is.numeric(.bValues))
        output(OL$Error, "Diffusion b-values must be given as a numeric vector")
    if (length(.bValues) != length(.gradientDirections))
        output(OL$Error, "An equal number of gradient subsets and b-values must be given")
    .gradientDirections <- lapply(.gradientDirections, validateGradientDirections)
    
    self <- list(
        expandComponents = function ()
        {
            returnValue <- list()
            returnValue$directions <- Reduce(cbind, scheme$getGradientDirections())
            returnValue$bValues <- rep(scheme$getBValues(), scheme$nDirections())
            return (returnValue)
        },
        
        getBValues = function () { return (.bValues) },
        
        getGradientDirections = function () { return (.gradientDirections) },
        
        nDirections = function () { return (sapply(.gradientDirections, ncol)) },
        
        summarise = function ()
        {
            output(OL$Info, "Number of gradient directions: ", implode(self$nDirections(),", "))
            output(OL$Info, "Diffusion b-values           : ", implode(round(.bValues),", "), " s/mm^2")
        }
    )
    
    class(self) <- c("scheme.diffusion.simple", "list.object", "list")
    invisible (self)
}

isSimpleDiffusionScheme <- function (object)
{
    return ("scheme.diffusion.simple" %in% class(object))
}

validateGradientDirections <- function (directions)
{
    if (!is.matrix(directions))
        output(OL$Error, "Gradient directions must be specified in a matrix")
    else if (ncol(directions) != 3 && nrow(directions) != 3)
        output(OL$Error, "Gradient directions should be specified as a 3xN matrix")
    else if (ncol(directions) == 3 && nrow(directions) != 3)
    {
        output(OL$Info, "Transposing gradient direction matrix")
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
        invisible(.SimpleDiffusionScheme(bValues, gradientDirections))
    else if (is.matrix(directions))
    {
        if (ncol(directions) == 3 && nrow(directions) != 3)
        {
            output(OL$Info, "Transposing gradient direction matrix")
            directions <- t(directions)
        }
        
        # Find groups with the same b-value
        bValueRunLengths <- rle(bValues)
        subsetBreaks <- cumsum(c(0, bValueRunLengths$lengths))
        gradientDirections <- lapply(seq_along(bValueRunLengths$lengths), function (i) directions[,(subsetBreaks[i]+1):subsetBreaks[i+1],drop=FALSE])
        
        invisible(.SimpleDiffusionScheme(bValueRunLengths$values, gradientDirections))
    }
    else
        output(OL$Error, "Gradient directions must be specified as a matrix or list of matrices")
}

newSimpleDiffusionSchemeFromSession <- function (session)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    directory <- session$getPreBedpostDirectory()
    bValues <- unlist(read.table(file.path(directory, "bvals")))
    directions <- as.matrix(read.table(file.path(directory, "bvecs")))
    names(bValues) <- NULL
    dimnames(directions) <- NULL
    
    scheme <- newSimpleDiffusionSchemeWithDirections(directions, bValues)
    invisible(scheme)
}

writeSimpleDiffusionSchemeForSession <- function (session, scheme)
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    if (!isSimpleDiffusionScheme(scheme))
        output(OL$Error, "Specified scheme is not a SimpleDiffusionScheme object")
    
    fslDir <- session$getPreBedpostDirectory()
    caminoDir <- session$getCaminoDirectory(createIfMissing=FALSE)
    
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
