#' Deprecated functions
#' 
#' These functions are deprecated, generally in favour of more succint
#' alternatives.
#' 
#' @rdname tractor.base-deprecated
#' @inheritParams readDicomDirectory
#' @export
newMriImageFromDicomDirectory <- function (dicomDir, readDiffusionParams = FALSE, untileMosaics = TRUE)
{
    .Deprecated("readDicomDirectory", "tractor.base")
    readDicomDirectory(dicomDir, readDiffusionParams, untileMosaics)
}

#' @rdname tractor.base-deprecated
#' @inheritParams readImageFile
#' @export
newMriImageFromFile <- function (fileName, fileType = NULL, metadataOnly = FALSE, volumes = NULL, sparse = FALSE, mask = NULL, reorder = TRUE)
{
    .Deprecated("readImageFile", "tractor.base")
    readImageFile(fileName, fileType, metadataOnly, volumes, sparse, mask, reorder)
}

#' @rdname tractor.base-deprecated
#' @inheritParams writeImageFile
#' @export
writeMriImageToFile <- function (image, fileName = NULL, fileType = NA, overwrite = TRUE)
{
    .Deprecated("writeImageFile", "tractor.base")
    writeImageFile(image, fileName, fileType, overwrite)
}
