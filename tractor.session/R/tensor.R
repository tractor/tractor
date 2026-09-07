#' Create a diffusion tensor matrix from its unique components
#'
#' This function creates a symmetric 3x3 diffusion tensor matrix from its six
#' unique components, as returned (per voxel) by [estimateDiffusionTensors()].
#'
#' @param components A numeric vector of length 6, giving the unique elements
#'   of the tensor in the order Dxx, Dxy, Dxz, Dyy, Dyz, Dzz.
#' @return A symmetric 3x3 numeric matrix representing the diffusion tensor.
#' @author Jon Clayden
#' @export
createDiffusionTensorFromComponents <- function (components)
{
    data <- components[c(1,2,3,2,4,5,3,5,6)]
    tensor <- matrix(data, nrow=3, ncol=3)
    return (tensor)
}

#' Fit diffusion tensors to a set of data
#'
#' This function fits diffusion tensors to one or more voxels' worth of
#' diffusion-weighted signal, by ordinary or iterative weighted least-squares
#' regression on the log-transformed data. It provides a pure-R alternative
#' to fitting tensors with FSL's `dtifit` (see [runDtifitWithSession()]).
#'
#' @param data A numeric vector or matrix of diffusion-weighted signal
#'   intensities, with one row per voxel (or a single vector for one voxel)
#'   and one column per gradient direction/b-value combination, matching
#'   `scheme`.
#' @param scheme A [tractor.base::DiffusionScheme] object, or a numeric
#'   b-matrix (one row per acquisition, with elements Bxx, Bxy, Bxz, Byy, Byz,
#'   Bzz).
#' @param method A string, either `"ls"` for ordinary least-squares (the
#'   default) or `"iwls"` for iterative weighted least-squares, which
#'   generally gives a more accurate fit at increased computational cost.
#' @param requireMetrics Boolean value: should tensor eigensystems and the
#'   derived mean diffusivity and fractional anisotropy metrics also be
#'   calculated?
#' @param convergenceLevel The relative change in weighted sum-of-squares
#'   below which the `"iwls"` iteration is considered to have converged.
#' @return A list with elements `logS0` (fitted log baseline signal per
#'   voxel), `tensors` (a 6 x N matrix of unique tensor components per voxel),
#'   `sse` (sum of squared residuals per voxel) and `bad` (the number of
#'   nonpositive data values per voxel). If `requireMetrics` is `TRUE`, this
#'   also includes `eigenvalues`, `eigenvectors`, `md` (mean diffusivity) and
#'   `fa` (fractional anisotropy).
#' @seealso [createDiffusionTensorFromComponents()], which converts a single
#'   voxel's unique tensor components into a matrix, and
#'   [createDiffusionTensorImagesForSession()], which applies this function
#'   across a whole session and writes the resulting maps to file.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
estimateDiffusionTensors <- function (data, scheme, method = c("ls","iwls"), requireMetrics = TRUE, convergenceLevel = 1e-3)
{
    if (!is(scheme, "DiffusionScheme") && !is.matrix(scheme))
        report(OL$Error, "The specified scheme object is not valid")
    
    method <- match.arg(method)
    
    data <- promote(data, byrow=TRUE)
    invalid <- which(data <= 0)
    bad <- rowSums(data <= 0, na.rm=TRUE)
    if (length(invalid) > 0)
    {
        report(OL$Warning, "Data contains ", length(invalid), " nonpositive values - these will be ignored")
        data[invalid] <- NA
    }
    logData <- log(data)
    rm(data)
    
    if (is.matrix(scheme))
        bMatrix <- (-scheme)
    else
    {
        bMatrix <- t(apply(scheme$getGradientDirections(), 1, function (row) {
            mat <- row %o% row
            return (c(mat[1,1], 2*mat[1,2], 2*mat[1,3], mat[2,2], 2*mat[2,3], mat[3,3]))
        }))
        bMatrix <- bMatrix * (-scheme$getBValues())
    }
    
    report(OL$Info, "Fitting tensors by ordinary least-squares", ifelse(method=="iwls"," (for initialisation)",""))
    if (length(invalid) > 0)
    {
        # If there are missing values we need to fit voxels one at a time
        ordinaryLeastSquaresFit <- function (i)
        {
            if (sum(!is.na(logData[i,])) < 7)
                return (c(rep(NA,7), rep(NA,ncol(logData))))
            else
            {
                tempSolution <- suppressWarnings(lsfit(bMatrix, logData[i,]))
                return (c(tempSolution$coefficients, tempSolution$residuals))
            }
        }
        
        values <- sapply(1:nrow(logData), ordinaryLeastSquaresFit)
        solution <- list(coefficients=values[1:7,], residuals=values[-(1:7),])
    }
    else
        solution <- lsfit(bMatrix, t(logData))
    
    if (method == "iwls")
    {
        weightedLeastSquaresFit <- function (i)
        {
            if (sum(!is.na(logData[i,])) < 7)
                return (c(rep(NA,7), rep(NA,ncol(logData))))
            else
            {
                voxelLogData <- logData[i,]
                voxelResiduals <- solution$residuals[,i]
                previousSumOfSquares <- sum(voxelResiduals^2, na.rm=TRUE)
                sumOfSquaresChange <- Inf
                tempSolution <- NULL
                
                iteration <- 1
                while (sumOfSquaresChange > convergenceLevel)
                {
                    # Weights are simply the predicted signals; nonpositive data values get zero weight
                    weights <- exp(voxelLogData - voxelResiduals)
                    weights[!is.finite(weights)] <- 0
                    tempSolution <- suppressWarnings(lsfit(bMatrix, voxelLogData, wt=weights))
                    voxelResiduals <- tempSolution$residuals
                    
                    weightedSumOfSquares <- sum(tempSolution$residuals^2 * weights, na.rm=TRUE)
                    # Exact fit: probably precisely seven valid directions
                    if (weightedSumOfSquares < sqrt(.Machine$double.eps))
                        break
                    if (iteration == 30)
                    {
                        flag(OL$Warning, "Iteration limit reached")
                        break
                    }
                    sumOfSquaresChange <- abs((previousSumOfSquares - weightedSumOfSquares) / previousSumOfSquares)
                    previousSumOfSquares <- weightedSumOfSquares
                    iteration <- iteration + 1
                }
            
                return (c(tempSolution$coefficients, tempSolution$residuals))
            }
        }
        
        report(OL$Info, "Applying iterative weighted least-squares")
        values <- sapply(1:nrow(logData), weightedLeastSquaresFit)
        
        solution$coefficients <- values[1:7,]
        solution$residuals <- values[-(1:7),]
    }
    
    coeffs <- promote(solution$coefficients)
    returnValue <- list(logS0=coeffs[1,], tensors=coeffs[2:7,,drop=FALSE], sse=colSums(promote(solution$residuals)^2,na.rm=TRUE), bad=bad)
    
    if (requireMetrics)
    {
        calculateEigensystem <- function (tensorComponents)
        {
            if (any(is.na(tensorComponents)))
                return (rep(NA, 12))
            else
            {
                tensor <- createDiffusionTensorFromComponents(tensorComponents)
                system <- eigen(tensor, symmetric=TRUE)
                return (c(system$values, system$vectors))
            }
        }
        calculateMetrics <- function (eigenvalues)
        {
            md <- mean(eigenvalues)
            fa <- sqrt(3/2) * vectorLength(eigenvalues-md) / vectorLength(eigenvalues)
            return (c(md,fa))
        }
        
        report(OL$Info, "Calculating tensor eigensystems")
        eigensystems <- apply(returnValue$tensors, 2, calculateEigensystem)
        metrics <- apply(eigensystems[1:3,,drop=FALSE], 2, calculateMetrics)
        
        returnValue <- c(returnValue, list(eigenvalues=eigensystems[1:3,], eigenvectors=array(eigensystems[4:12,],dim=c(3,3,nrow(logData))), md=metrics[1,], fa=metrics[2,]))
    }
    
    return (returnValue)
}

#' Fit diffusion tensors for a session and write metric maps
#'
#' This function fits diffusion tensors to a session's preprocessed
#' diffusion-weighted data, within its brain mask, using
#' [estimateDiffusionTensors()], and writes the resulting baseline signal,
#' fractional anisotropy, mean diffusivity, eigenvalue, eigenvector, radial
#' diffusivity, sum-of-squared-error, "bad voxel count" and colour-FA maps to
#' the session's diffusion directory. This provides a pure-R alternative to
#' fitting tensors with FSL's `dtifit` (see [runDtifitWithSession()]).
#'
#' @param session An [MriSession] object, which must already have a mask and
#'   preprocessed diffusion-weighted data associated with it.
#' @param method A string, either `"ls"` for ordinary least-squares (the
#'   default) or `"iwls"` for iterative weighted least-squares. Passed to
#'   [estimateDiffusionTensors()].
#' @return This function is called for its side effect.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
createDiffusionTensorImagesForSession <- function (session, method = c("ls","iwls"))
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    method <- match.arg(method)
    
    scheme <- session$getDiffusionScheme()
    
    maskImage <- session$getImageByType("mask", "diffusion")
    intraMaskLocs <- which(maskImage$getData() > 0, arr.ind=TRUE)
    imageDims <- maskImage$getDimensions()
    
    dataImage <- session$getImageByType("data", "diffusion")
    data <- apply(dataImage$getData(), 4, "[", intraMaskLocs)
    rm(dataImage)
    
    report(OL$Info, "Fitting diffusion tensors for ", nrow(intraMaskLocs), " voxels")
    fit <- estimateDiffusionTensors(data, scheme, method=method)
    
    scalarData <- array(NA, dim=imageDims)
    vectorData <- array(NA, dim=c(imageDims,3))
    
    writeMap <- function (values, name, vector = FALSE)
    {
        if (vector)
        {
            for (i in 1:3)
                vectorData[cbind(intraMaskLocs,i)] <- values[,i]
            image <- asMriImage(vectorData, maskImage, imageDims=c(imageDims,3), voxelDims=c(maskImage$getVoxelDimensions(),1), origin=c(maskImage$getOrigin(),0))
        }
        else
        {
            scalarData[intraMaskLocs] <- values
            image <- asMriImage(scalarData, maskImage)
        }
        
        writeImageFile(image, name)
    }
    
    report(OL$Info, "Writing tensor metric maps")
    writeMap(exp(fit$logS0), session$getImageFileNameByType("s0","diffusion"))
    writeMap(fit$fa, session$getImageFileNameByType("fa","diffusion"))
    writeMap(fit$md, session$getImageFileNameByType("md","diffusion"))
    for (i in 1:3)
    {
        writeMap(fit$eigenvalues[i,], session$getImageFileNameByType("eigenvalue","diffusion",index=i))
        writeMap(t(fit$eigenvectors[,i,]), session$getImageFileNameByType("eigenvector","diffusion",index=i), vector=TRUE)
    }
    writeMap((fit$eigenvalues[2,]+fit$eigenvalues[3,]) / 2, session$getImageFileNameByType("radialdiff","diffusion"))
    writeMap(fit$sse, session$getImageFileNameByType("sse","diffusion"))
    writeMap(fit$bad, file.path(session$getDirectory("diffusion"), "dti_bad"))
    
    boundedFA <- pmin(1, pmax(0, fit$fa))
    channels <- t(abs(fit$eigenvectors[,1,])) * boundedFA
    scalarData <- array(NA_integer_, dim=imageDims)
    colourFA <- RNifti::rgbArray(replace(scalarData,intraMaskLocs,channels[,1]), replace(scalarData,intraMaskLocs,channels[,2]), replace(scalarData,intraMaskLocs,channels[,3]))
    image <- asMriImage(colourFA, maskImage)
    writeImageFile(image, session$getImageFileNameByType("colourfa"))
}
