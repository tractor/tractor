#' Angles between corresponding rows of two matrices
#'
#' This function calculates the angle, using
#' [tractor.base::angleBetweenVectors()], between each pair of corresponding
#' rows in two matrices (or vectors, which are treated as single rows). If
#' the matrices have different numbers of rows, the result is padded with
#' `NA` to the length of the longer one.
#'
#' @param matrix1, matrix2 Numeric matrices (or vectors), whose rows are
#'   compared pairwise.
#' @return A numeric vector of angles, in radians, of length equal to the
#'   larger of the number of rows in `matrix1` and `matrix2`. The result is
#'   returned invisibly.
#' @author Jon Clayden
#' @export
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

#' Step vectors between successive points, from a seed
#'
#' This function calculates the vectors between successive rows of a matrix
#' of points, working outwards in each direction from a designated seed
#' point.
#'
#' @param points A numeric matrix of 3D point coordinates, one per row.
#' @param seedPoint An integer giving the row index of the seed point within
#'   `points`.
#' @return A list with elements `left` and `right`, each a matrix of step
#'   vectors moving outwards from the seed point towards the start and end of
#'   `points` respectively. The first row of each is `NA`, since there is no
#'   step vector at the seed itself. The result is returned invisibly.
#' @author Jon Clayden
#' @export
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

#' Characterise step vectors between successive points, from a seed
#'
#' This function calculates the step vectors between successive rows of a
#' matrix of points, as [calculateStepVectors()] does, and additionally
#' calculates the angles between consecutive step vectors on each side of the
#' seed point, and the angle between the first step vectors on either side.
#'
#' @param points A numeric matrix of 3D point coordinates, one per row.
#' @param seedPoint An integer giving the row index of the seed point within
#'   `points`.
#' @return A list with elements `leftVectors` and `rightVectors` (as `left`
#'   and `right` from [calculateStepVectors()]), `leftAngles` and
#'   `rightAngles`, numeric vectors of angles (in radians) between
#'   consecutive step vectors on each side, `middleAngle`, the angle between
#'   the first left and right step vectors, and `leftLength` and
#'   `rightLength`, the number of points on each side of (and including) the
#'   seed. The result is returned invisibly.
#' @author Jon Clayden
#' @export
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
