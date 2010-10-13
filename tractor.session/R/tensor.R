createDiffusionTensorFromComponents <- function (components)
{
    data <- components[c(1,2,3,2,4,5,3,5,6)]
    tensor <- matrix(data, nrow=3, ncol=3)
    return (tensor)
}

estimateDiffusionTensors <- function (data, scheme, method = c("ls","iwls"), requireMetrics = TRUE, convergenceLevel = 1e-2)
{
    if (!isSimpleDiffusionScheme(scheme))
        output(OL$Error, "The specified scheme object is not valid")
    
    method <- match.arg(method)
    
    data <- promote(data, byrow=TRUE)
    logData <- ifelse(data==0, 0, log(data))
    
    components <- scheme$expandComponents()
    bMatrix <- t(apply(components$directions, 2, function (column) {
        mat <- column %o% column
        return (c(mat[1,1], 2*mat[1,2], 2*mat[1,3], mat[2,2], 2*mat[2,3], mat[3,3]))
    }))
    bMatrix <- bMatrix * (-components$bValues)
    
    output(OL$Info, "Fitting tensors by ordinary least-squares", ifelse(method=="iwls"," (for initialisation)",""))
    solution <- lsfit(bMatrix, t(logData))
    
    if (method == "iwls")
    {
        weightedLeastSquaresFit <- function (i)
        {
            voxelLogData <- logData[i,]
            voxelResiduals <- solution$residuals[,i]
            previousSumOfSquares <- sum(voxelResiduals^2)
            sumOfSquaresChange <- Inf
            tempSolution <- NULL
            
            while (sumOfSquaresChange > convergenceLevel)
            {
                # Weights are simply the predicted signals
                tempSolution <- lsfit(bMatrix, voxelLogData, wt=exp(voxelLogData-voxelResiduals))
                voxelResiduals <- tempSolution$residuals
                
                sumOfSquares <- sum(tempSolution$residuals^2)
                sumOfSquaresChange <- abs((previousSumOfSquares - sumOfSquares) / previousSumOfSquares)
                previousSumOfSquares <- sumOfSquares
            }
            
            return (c(tempSolution$coefficients, tempSolution$residuals))
        }
        
        output(OL$Info, "Applying iterative weighted least-squares")
        values <- sapply(1:nrow(data), weightedLeastSquaresFit)
        
        solution$coefficients <- values[1:7,]
        solution$residuals <- values[-(1:7),]
    }
    
    coeffs <- promote(solution$coefficients)
    returnValue <- list(logS0=coeffs[1,], tensors=coeffs[2:7,,drop=FALSE], sse=colSums(solution$residuals^2))
    
    if (requireMetrics)
    {
        calculateEigensystem <- function (tensorComponents)
        {
            tensor <- createDiffusionTensorFromComponents(tensorComponents)
            system <- eigen(tensor, symmetric=TRUE)
            return (c(system$values, system$vectors))
        }
        calculateMetrics <- function (eigenvalues)
        {
            md <- mean(eigenvalues)
            fa <- sqrt(3/2) * vectorLength(eigenvalues-md) / vectorLength(eigenvalues)
            return (c(md,fa))
        }
        
        output(OL$Info, "Calculating tensor eigensystems")
        eigensystems <- apply(returnValue$tensors, 2, calculateEigensystem)
        metrics <- apply(eigensystems[1:3,,drop=FALSE], 2, calculateMetrics)
        
        returnValue <- c(returnValue, list(eigenvalues=eigensystems[1:3,], eigenvectors=array(eigensystems[4:12,],dim=c(3,3,nrow(data))), md=metrics[1,], fa=metrics[2,]))
    }
    
    return (returnValue)
}

createDiffusionTensorImagesForSession <- function (session, method = c("ls","iwls"))
{
    if (!isMriSession(session))
        output(OL$Error, "Specified session is not an MriSession object")
    
    method <- match.arg(method)
    
    scheme <- newSimpleDiffusionSchemeFromSession(session)
    
    maskImage <- session$getImageByType("mask")
    intraMaskLocs <- which(maskImage$getData() > 0, arr.ind=TRUE)
    imageDims <- maskImage$getDimensions()
    
    dataImage <- newMriImageFromFile(file.path(session$getPreBedpostDirectory(), "data"))
    data <- apply(dataImage$getData(), 4, "[", intraMaskLocs)
    rm(dataImage)
    
    output(OL$Info, "Fitting diffusion tensors for ", nrow(intraMaskLocs), " voxels")
    fit <- estimateDiffusionTensors(data, scheme, method=method)
    
    scalarMetadata <- newMriImageMetadataFromTemplate(maskImage$getMetadata(), datatype=getDataTypeByNiftiCode(16))
    scalarData <- array(NA, dim=imageDims)
    vectorMetadata <- newMriImageMetadataFromTemplate(maskImage$getMetadata(), imageDims=c(imageDims,3), voxelDims=c(maskImage$getVoxelDimensions(),1), origin=c(maskImage$getOrigin(),0), datatype=getDataTypeByNiftiCode(16))
    vectorData <- array(NA, dim=c(imageDims,3))
    
    targetDir <- session$getPreBedpostDirectory()
    
    writeMap <- function (values, name, vector = FALSE)
    {
        if (vector)
        {
            for (i in 1:3)
                vectorData[cbind(intraMaskLocs,i)] <- values[,i]
            image <- newMriImageWithData(vectorData, vectorMetadata)
        }
        else
        {
            scalarData[intraMaskLocs] <- values
            image <- newMriImageWithData(scalarData, scalarMetadata)
        }
        
        writeMriImageToFile(image, file.path(targetDir,name))
    }
    
    output(OL$Info, "Writing tensor metric maps")
    writeMap(exp(fit$logS0), "dti_S0")
    writeMap(fit$fa, "dti_FA")
    writeMap(fit$md, "dti_MD")
    for (i in 1:3)
    {
        writeMap(fit$eigenvalues[i,], paste("dti_L",i,sep=""))
        writeMap(t(fit$eigenvectors[,i,]), paste("dti_V",i,sep=""), vector=TRUE)
    }
    writeMap((fit$eigenvalues[2,]+fit$eigenvalues[3,]) / 2, "dti_Lrad")
    writeMap(fit$sse, "dti_sse")
    writeMap(fit$sse / (fit$logS0^2), "dti_sse_scaled")
}
