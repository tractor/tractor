.SimpleDiffusionScheme <- function (.bValue, .gradientDirections)
{
    if (is.matrix(.gradientDirections))
        .gradientDirections <- list(.gradientDirections)
    if (!is.numeric(.bValues))
        output(OL$Error, "Diffusion b-values must be given as a numeric vector")
    if (length(.bValues) != length(.gradientDirections))
        output(OL$Error, "An equal number of gradient subsets and b-values must be given")
    .gradientDirections <- lapply(.gradientDirections, validateGradientDirections)
    
    self <- list(
        getBValues = function () { return (.bValues) },
        
        getGradientDirections = function () { return (.gradientDirections) },
        
        nDirections = function () { return (sapply(.gradientDirections, ncol)) },
        
        summarise = function ()
        {
            output(OL$Info, "Number of gradient directions: ", implode(self$nDirections(),", "))
            output(OL$Info, "Diffusion b-values           : ", implode(round(.bValues/1e6),", "), " s/mm^2")
        }
    )
    
    class(self) <- c("scheme.diffusion.simple", "list.object", "list")
    invisible (self)
}

isSimpleDiffusionScheme <- function (object)
{
    return ("scheme.diffusion.simple" %in% class(object))
}

newSimpleDiffusionSchemeWithDirections <- function (gradientDirections, bValues)
{
    invisible(.SimpleDiffusionScheme(bValues, gradientDirections))
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
    directions <- apply(directions, 2, function (x) x/vectorLength(x))
    
    return (directions)
}
