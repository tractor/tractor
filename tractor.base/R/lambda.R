#' Shorthand anonymous functions
#' 
#' These functions provide a shorthand route to simple anonymous functions.
#' 
#' @param expr A (single or compound) expression forming the body of the
#'   function.
#' @return The function constructed.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18.
#' \url{http://www.jstatsoft.org/v44/i08/}.
#' @rdname lambda
#' @export
fx <- function (expr)
{
    f <- function(x) {}
    body(f) <- substitute(expr)
    return (f)
}

#' @rdname lambda
#' @export
fxy <- function (expr)
{
    f <- function(x,y) {}
    body(f) <- substitute(expr)
    return (f)
}

#' @rdname lambda
#' @export
fxyz <- function (expr)
{
    f <- function(x,y,z) {}
    body(f) <- substitute(expr)
    return (f)
}

#' @rdname lambda
#' @export
fi <- function (expr)
{
    f <- function(i) {}
    body(f) <- substitute(expr)
    return (f)
}
