evaluateDistribution <- function (x, params, log = FALSE)
{
    if ("distribution.beta" %in% class(params))
        return (evaluateBetaDistribution(x, params, log))
    if ("distribution.gaussian" %in% class(params))
        return (evaluateGaussianDistribution(x, params, log))
    if ("distribution.multinomial" %in% class(params))
        return (evaluateMultinomialDistribution(x, params, log))
}

fitBetaDistribution <- function (data, alpha = NULL, beta = 1, weights = NULL)
{
    if (!is.vector(data))
        output(OL$Error, "Beta distribution fit requires a vector of data")
        
    if (is.null(weights))
        weights <- rep(1, length(data))
    else if (length(weights) != length(data))
        output(OL$Error, "Data and weight vectors must have the same length")
    
    data <- data[!is.na(weights)]
    weights <- weights[!is.na(weights)]
    
    if (is.null(alpha) && (beta == 1))
        alpha <- (-sum(weights)) / sum(weights*log(data))
    
    result <- list(alpha=alpha, beta=beta)
    class(result) <- c("distribution.beta", "list")
    return (result)
}

evaluateBetaDistribution <- function (x, params, log = FALSE)
{
    return (dbeta(x, params$alpha, params$beta, ncp=0, log=log))
}

fitRegularisedBetaDistribution <- function (data, alpha = NULL, beta = 1, lambda = 1, weights = NULL)
{
    if (is.null(weights))
        weightSum <- length(data)
    else
        weightSum <- sum(weights, na.rm=TRUE)
    
    result <- fitBetaDistribution(data, alpha, beta, weights)
    if (!is.null(lambda) && is.null(alpha) && (beta == 1))
        result$alpha <- (result$alpha*weightSum) / (result$alpha*lambda + weightSum)
    
    return (result)
}

fitGaussianDistribution <- function (data, mu = NULL, sigma = NULL)
{
    if (!is.vector(data))
        output(OL$Error, "Gaussian distribution fit requires a vector of data")
    if (length(data) == 0)
        output(OL$Error, "Data vector is empty!")
    
    if (is.null(mu))
        mu <- mean(data)
    if (is.null(sigma))
        sigma <- sqrt(sum((data - mu)^2) / length(data))
	
    result <- list(mu=mu, sigma=sigma)
    class(result) <- c("distribution.gaussian", "list")
    return (result)
}

evaluateGaussianDistribution <- function (x, params, log = FALSE)
{
    return (dnorm(x, mean=params$mu, sd=params$sigma, log=log))
}

fitMultinomialDistribution <- function (data, const = 0, values = NULL, weights = NULL)
{
    if (!is.vector(data))
        output(OL$Error, "Multinomial distribution fit requires a vector of data")
    
    if (is.null(weights))
        weights <- rep(1, length(data))
    else if (length(weights) != length(data))
        output(OL$Error, "Data and weight vectors must have the same length")
    
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
            output(OL$Error, "Some multinomial fit data are not amongst the specified allowable values")
        counts[locs] <- as.vector(hist)
        counts <- counts + const
    }
    
    probs <- counts / sum(counts)
    result <- list(probs=probs, values=values)
    class(result) <- c("distribution.multinomial", "list")
    return (result)
}

evaluateMultinomialDistribution <- function (x, params, log = FALSE)
{
    if (is.na(x))
        return (NA)
    if (!is.numeric(x))
        output(OL$Error, "Multinomial data must be numeric")
    
    if (length(x) == length(params$probs))
        return (dmultinom(x, prob=params$probs, log=log))
    else if (length(x) == 1)
    {
        y <- rep(0, length(params$probs))
        loc <- which(params$values == x)
        if (length(loc) != 1)
        {
            loc <- which.min(abs(params$values - x))
            output(OL$Warning, "The specified value (", x, ") is not valid for this distribution; treating as ", params$values[loc])
        }
        
        y[loc] <- 1
        return (dmultinom(y, size=1, prob=params$probs, log=log))
    }
    else
        output(OL$Error, "Multinomial data must be specified as a single number or full vector of frequencies")
}
