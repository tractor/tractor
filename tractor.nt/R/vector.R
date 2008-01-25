anglesBetweenMatrices <- function (matrix1, matrix2)
{
    matrix1 <- promote(matrix1, byrow=TRUE)
    matrix2 <- promote(matrix2, byrow=TRUE)
    
    lengths <- c(nrow(matrix1), nrow(matrix2))
    angles <- rep(NA, max(lengths))
    
    for (i in seq_len(min(lengths)))
        angles[i] <- angleBetweenVectors(matrix1[i,], matrix2[i,])
    
    invisible (angles)
}

calculateStepVectors <- function (points, seedPoint)
{
    nPoints <- nrow(points)
    
    if (seedPoint > 1)
        leftVectors <- rbind(rep(NA,3), diff(points[seedPoint:1,]))
    else
        leftVectors <- matrix(rep(NA,3), nrow=1)
    if (seedPoint < nPoints)
        rightVectors <- rbind(rep(NA,3), diff(points[seedPoint:nPoints,]))
    else
        rightVectors <- matrix(rep(NA,3), nrow=1)

    invisible (list(left=leftVectors, right=rightVectors))
}

characteriseStepVectors <- function (points, seedPoint)
{
    vectors <- calculateStepVectors(points, seedPoint)
    
    leftLength <- nrow(vectors$left)
    if (leftLength > 2)
        leftAngles <- c(NA, NA, anglesBetweenMatrices(vectors$left[2:(leftLength-1),], vectors$left[3:leftLength,]))
    else
        leftAngles <- rep(NA, leftLength)
    
    rightLength <- nrow(vectors$right)
    if (rightLength > 2)
        rightAngles <- c(NA, NA, anglesBetweenMatrices(vectors$right[2:(rightLength-1),], vectors$right[3:rightLength,]))
    else
        rightAngles <- rep(NA, rightLength)
    
    if ((leftLength > 1) && (rightLength > 1))
        middleAngle <- angleBetweenVectors(-vectors$left[2,], vectors$right[2,])
    else
        middleAngle <- NA
    
    invisible (list(leftVectors=vectors$left, rightVectors=vectors$right, leftAngles=leftAngles, rightAngles=rightAngles, middleAngle=middleAngle, leftLength=leftLength, rightLength=rightLength))
}
