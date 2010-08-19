.SimpleDiffusionScheme <- function (.bValue, .gradientDirections)
{
    if (is.matrix(.gradientDirections))
        .gradientDirections <- list(.gradientDirections)
    .gradientDirections <- lapply(.gradientDirections, validateGradientDirections)
    
    self <- list(
        getBValue = function () { return (.bValue) },
        
        getGradientDirections = function () { return (.gradientDirections) },
        
        nDirections = function () { return (sapply(.gradientDirections, ncol)) },
        
        summarise = function ()
        {
            output(OL$Info, "Number of gradient directions: ", implode(self$nDirections(),", "))
            output(OL$Info, "Diffusion b-values           : ", implode(round(.bValue/1e6),", "), " s/mm^2")
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
    
    return (directions)
}
