#' Evaluate a fitted distribution
#'
#' This function evaluates the density of a distribution object previously
#' fitted by one of [fitBetaDistribution()], [fitGaussianDistribution()] or
#' [fitMultinomialDistribution()] (or their variants), dispatching to the
#' appropriate specific evaluation function based on the class of `params`.
#'
#' @param x A numeric vector of values at which to evaluate the density.
#' @param params A `betaDistribution`, `gaussianDistribution` or
#'   `multinomialDistribution` object.
#' @param log Boolean value: if `TRUE`, log-densities are returned.
#' @return A numeric vector of (log-)densities.
#' @author Jon Clayden
#' @export
evaluateDistribution <- function (x, params, log = FALSE)
{
    if (is(params, "betaDistribution"))
        return (evaluateBetaDistribution(x, params, log))
    else if (is(params, "gaussianDistribution"))
        return (evaluateGaussianDistribution(x, params, log))
    else if (is(params, "multinomialDistribution"))
        return (evaluateMultinomialDistribution(x, params, log))
}

#' Fit a beta distribution
#'
#' This function fits a beta distribution to data lying in the range
#' \eqn{[0,1]}, such as the rescaled cosine similarities produced by
#' [calculateRescaledCosinesFromAngles()]. If `alpha` is not specified and
#' `beta` is 1, the (weighted) maximum-likelihood estimate of `alpha` is
#' calculated in closed form.
#'
#' @param data A numeric vector of data values in the range \eqn{[0,1]}.
#' @param alpha The alpha (shape one) parameter of the distribution, or
#'   `NULL` to estimate it from `data` (only possible if `beta` is 1).
#' @param beta The beta (shape two) parameter of the distribution.
#' @param weights An optional numeric vector of weights, of the same length
#'   as `data`, giving the contribution of each data point to the fit.
#'   Elements of `data` corresponding to missing weights are dropped. The
#'   default, `NULL`, weights all data points equally.
#' @return A `betaDistribution` object, a list with elements `alpha` and
#'   `beta`.
#' @seealso [evaluateBetaDistribution()], which evaluates the density of a
#'   fitted distribution, and [fitRegularisedBetaDistribution()], which adds
#'   shrinkage to the estimate of `alpha`.
#' @author Jon Clayden
#' @export
fitBetaDistribution <- function (data, alpha = NULL, beta = 1, weights = NULL)
{
    if (!is.vector(data))
        report(OL$Error, "Beta distribution fit requires a vector of data")
        
    if (is.null(weights))
        weights <- rep(1, length(data))
    else if (length(weights) != length(data))
        report(OL$Error, "Data and weight vectors must have the same length")
    
    data <- data[!is.na(weights)]
    weights <- weights[!is.na(weights)]
    
    if (is.null(alpha) && (beta == 1))
        alpha <- (-sum(weights)) / sum(weights*log(data))
    
    result <- list(alpha=alpha, beta=beta)
    class(result) <- "betaDistribution"
    return (result)
}

#' Evaluate a fitted beta distribution
#'
#' This function evaluates the density of a beta distribution previously fit
#' with [fitBetaDistribution()] or [fitRegularisedBetaDistribution()].
#'
#' @param x A numeric vector of values, in the range \eqn{[0,1]}, at which to
#'   evaluate the density.
#' @param params A `betaDistribution` object.
#' @param log Boolean value: if `TRUE`, log-densities are returned.
#' @return A numeric vector of (log-)densities.
#' @author Jon Clayden
#' @export
evaluateBetaDistribution <- function (x, params, log = FALSE)
{
    if (!is(params, "betaDistribution"))
        report(OL$Error, "The specified object does not describe a beta distribution")
    return (dbeta(x, params$alpha, params$beta, ncp=0, log=log))
}

#' Fit a regularised beta distribution
#'
#' This function fits a beta distribution as [fitBetaDistribution()] does,
#' but with the estimated alpha parameter shrunk towards 1 (an uninformative,
#' uniform distribution) and optionally offset. This regularisation is useful
#' when, as in probabilistic neighbourhood tractography (PNT), there may be
#' relatively little data available to fit each distribution.
#'
#' @param data A numeric vector of data values in the range \eqn{[0,1]}.
#' @param alpha The alpha (shape one) parameter of the distribution, or
#'   `NULL` to estimate it from `data` (only possible if `beta` is 1).
#' @param beta The beta (shape two) parameter of the distribution.
#' @param lambda The regularisation parameter. Larger values shrink the
#'   estimated alpha further towards 1. Setting this to `NULL` disables
#'   regularisation, falling back to the estimate from [fitBetaDistribution()].
#' @param alphaOffset A number added to the (shrunk) estimate of alpha.
#' @param weights An optional numeric vector of weights, of the same length
#'   as `data`, giving the contribution of each data point to the fit, and to
#'   the effective sample size used in shrinkage. The default, `NULL`,
#'   weights all data points equally.
#' @return A `betaDistribution` object, a list with elements `alpha` and
#'   `beta`.
#' @author Jon Clayden
#' @export
fitRegularisedBetaDistribution <- function (data, alpha = NULL, beta = 1, lambda = 1, alphaOffset = 0, weights = NULL)
{
    if (is.null(weights))
        weightSum <- 1
    else
        weightSum <- sum(weights, na.rm=TRUE)
    
    result <- fitBetaDistribution(data, alpha, beta, weights)
    
    # NB: The expression used to calculate alpha here is not self-normalising,
    # so the relative scales of "weights" and "lambda" matter
    # This is required to allow for multiple data points from a single subject,
    # but care must be taken if using this function for other purposes
    if (!is.null(lambda) && is.null(alpha) && (beta == 1))
        result$alpha <- (result$alpha*weightSum) / (result$alpha*lambda + weightSum) + alphaOffset
    
    return (result)
}

#' Fit a Gaussian distribution
#'
#' This function fits a Gaussian (normal) distribution to a vector of data by
#' maximum likelihood, i.e. using the sample mean and (biased) sample
#' standard deviation.
#'
#' @param data A numeric vector of data values.
#' @param mu The mean of the distribution, or `NULL` to estimate it from
#'   `data`.
#' @param sigma The standard deviation of the distribution, or `NULL` to
#'   estimate it from `data`.
#' @return A `gaussianDistribution` object, a list with elements `mu` and
#'   `sigma`.
#' @author Jon Clayden
#' @export
fitGaussianDistribution <- function (data, mu = NULL, sigma = NULL)
{
    if (!is.vector(data))
        report(OL$Error, "Gaussian distribution fit requires a vector of data")
    if (length(data) == 0)
        report(OL$Error, "Data vector is empty!")
    
    if (is.null(mu))
        mu <- mean(data)
    if (is.null(sigma))
        sigma <- sqrt(sum((data - mu)^2) / length(data))
	
    result <- list(mu=mu, sigma=sigma)
    class(result) <- "gaussianDistribution"
    return (result)
}

#' Evaluate a fitted Gaussian distribution
#'
#' This function evaluates the density of a Gaussian distribution previously
#' fit with [fitGaussianDistribution()].
#'
#' @param x A numeric vector of values at which to evaluate the density.
#' @param params A `gaussianDistribution` object.
#' @param log Boolean value: if `TRUE`, log-densities are returned.
#' @return A numeric vector of (log-)densities.
#' @author Jon Clayden
#' @export
evaluateGaussianDistribution <- function (x, params, log = FALSE)
{
    if (!is(params, "gaussianDistribution"))
        report(OL$Error, "The specified object does not describe a Gaussian distribution")
    return (dnorm(x, mean=params$mu, sd=params$sigma, log=log))
}

#' Fit a multinomial distribution
#'
#' This function fits a multinomial distribution over a discrete set of
#' values, such as candidate tract lengths, to a vector of data, using
#' (weighted) observed frequencies and optional Laplace-style smoothing.
#'
#' @param data A numeric vector of data values.
#' @param const A number added to each (weighted) observed count before
#'   normalising to probabilities, providing Laplace-style smoothing. The
#'   default, 0, applies no smoothing.
#' @param values A numeric vector giving the allowable values that `data` may
#'   take, defining the support of the fitted distribution. The default,
#'   `NULL`, uses only the values observed in `data`. An error results if
#'   `data` contains a value not present in `values`.
#' @param weights An optional numeric vector of weights, of the same length
#'   as `data`, giving the contribution of each data point to the fit.
#'   Elements of `data` corresponding to missing weights are dropped. The
#'   default, `NULL`, weights all data points equally.
#' @return A `multinomialDistribution` object, a list with elements `probs`
#'   and `values`.
#' @author Jon Clayden
#' @export
fitMultinomialDistribution <- function (data, const = 0, values = NULL, weights = NULL)
{
    if (!is.vector(data))
        report(OL$Error, "Multinomial distribution fit requires a vector of data")
    
    if (is.null(weights))
        weights <- rep(1, length(data))
    else if (length(weights) != length(data))
        report(OL$Error, "Data and weight vectors must have the same length")
    
    data <- data[!is.na(weights)]
    weights <- weights[!is.na(weights)]
    
    hist <- tapply(weights, factor(data), "sum")
    dataValues <- as.numeric(names(hist))
    
    if (is.null(values))
    {
        values <- dataValues
        counts <- as.vector(hist) + const
    }
    else
    {
        counts <- rep(0, length(values))
        locs <- match(dataValues, values)
        if (sum(is.na(locs)) != 0)
            report(OL$Error, "Some multinomial fit data are not amongst the specified allowable values")
        counts[locs] <- as.vector(hist)
        counts <- counts + const
    }
    
    probs <- counts / sum(counts)
    result <- list(probs=probs, values=values)
    class(result) <- "multinomialDistribution"
    return (result)
}

#' Evaluate a fitted multinomial distribution
#'
#' This function evaluates the probability mass of a multinomial
#' distribution, previously fit with [fitMultinomialDistribution()], at a
#' single value or a full vector of observed frequencies.
#'
#' @param x A single number, one of the values in `params$values` (if not,
#'   the nearest value is used, with a warning), or a numeric vector of
#'   frequencies of the same length as `params$probs`. `NA` is returned
#'   unmodified.
#' @param params A `multinomialDistribution` object.
#' @param log Boolean value: if `TRUE`, the log-probability is returned.
#' @return A single (log-)probability.
#' @author Jon Clayden
#' @export
evaluateMultinomialDistribution <- function (x, params, log = FALSE)
{
    if (!is(params, "multinomialDistribution"))
        report(OL$Error, "The specified object does not describe a multinomial distribution")
    
    if (is.na(x))
        return (NA)
    if (!is.numeric(x))
        report(OL$Error, "Multinomial data must be numeric")
    
    if (length(x) == length(params$probs))
        return (dmultinom(x, prob=params$probs, log=log))
    else if (length(x) == 1)
    {
        y <- rep(0, length(params$probs))
        loc <- which(params$values == x)
        if (length(loc) != 1)
        {
            loc <- which.min(abs(params$values - x))
            report(OL$Warning, "The specified value (", x, ") is not valid for this distribution; treating as ", params$values[loc])
        }
        
        y[loc] <- 1
        return (dmultinom(y, size=1, prob=params$probs, log=log))
    }
    else
        report(OL$Error, "Multinomial data must be specified as a single number or full vector of frequencies")
}
