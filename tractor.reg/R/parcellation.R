#' Read and write parcellations
#' 
#' A parcellation is an integer-valued image in which each unique value
#' represents a labelled region. The image is therefore associated with some
#' metadata about each region, which may include a name, a tissue type and a
#' colour to use when visualising the image. TractoR stores this metadata in
#' sidecar files with a `"lut"` file extension, structured as a table. These
#' functions read and write this format alongside the associated image.
#' 
#' @param imageFileName Path to an image file.
#' @param regionFileName Path to the associated region metadata file. If
#'   `NULL`, this will default to the same path as the image but with a `"lut"`
#'   file extension.
#' @param parcellation A list object representing a parcellation, as produced
#'   by `readParcellation`.
#' @param ... Additional arguments to [tractor.base::readImageFile()] or
#'   [tractor.base::writeImageFile()], as appropriate.
#' @return For `readParcellation`, a list with components
#'   - `image`: an `MriImage` object representing the image, and
#'   - `regions`: a `data.frame` containing the region metadata.
#'   `writeParcellation` is called for its side-effect.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @rdname parcellations
#' @export
readParcellation <- function (imageFileName, regionFileName = NULL, ...)
{
    image <- readImageFile(imageFileName, ...)
    if (is.null(regionFileName))
        regionFileName <- ensureFileSuffix(image$getSource(), "lut")
    regions <- read.table(regionFileName, header=TRUE, stringsAsFactors=FALSE)
    return (list(image=image, regions=regions))
}

#' @rdname parcellations
#' @export
writeParcellation <- function (parcellation, ...)
{
    image <- parcellation$image
    regions <- parcellation$regions
    
    if (!is.data.frame(regions))
        report(OL$Error, "Regions should be specified as a data frame")
    else if (nrow(regions) == 0)
        report(OL$Error, "Regions data frame is empty")
    
    indicesPresent <- unique(as.vector(image$getData()))
    missing <- which(!(regions$index %in% indicesPresent))
    if (length(missing) > 0)
        regions <- regions[-missing,,drop=FALSE]
    
    info <- writeImageFile(image, ...)
    regionFileName <- ensureFileSuffix(info$fileStem, "lut")
    regionMatrix <- as.matrix(format(regions, justify="left"))
    regionMatrix[,"colour"] <- paste('"', regionMatrix[,"colour"], '"', sep="")
    
    nColumns <- ncol(regions)
    widths <- pmax(nchar(regionMatrix[1,]), nchar(colnames(regions)))
    
    padding <- widths - nchar(colnames(regions))
    headerString <- " "
    for (i in seq_len(nColumns))
        headerString <- paste(headerString, colnames(regions)[i], implode(rep(" ",padding[i]+2),sep=""), sep="")
    headerString <- substr(headerString, 1, nchar(headerString)-2)
    
    rightJustify <- sapply(seq_len(nColumns), function(i) is.numeric(regions[,i]))
    padding <- widths - nchar(regionMatrix[1,])
    lines <- apply(regionMatrix, 1, function(x) {
        string <- " "
        for (i in seq_along(x))
        {
            if (rightJustify[i])
                string <- paste(string, implode(rep(" ",padding[i]),sep=""), x[i], "  ", sep="")
            else
                string <- paste(string, x[i], implode(rep(" ",padding[i]+2),sep=""), sep="")
        }
        return (string)
    })
    lines <- c(headerString, implode(c("#",rep("-",sum(widths)+2*(length(widths)-1)))), lines)
    lines <- sub("\\s+$", "", lines, perl=TRUE)
    
    writeLines(lines, regionFileName)
}

#' Match regions to a parcellation
#' 
#' This function matches region names or indices to values within a
#' parcellation. Integer-valued region specifications (including number
#' strings) are matched against region indices; other names are matched to
#' regions by label, lobe, tissue type and hemisphere in that order of
#' preference.
#' 
#' @param regions Region names and/or integer indices. Each value is matched at
#'   only one level, but regions are included in the return value if they match
#'   on any term.
#' @param parcellation A list object representing a parcellation, as produced
#'   by [readParcellation()].
#' @param labels Boolean value. If `TRUE`, the labels of the matching regions
#'   are returned; otherwise the integer indices.
#' @return Values corresponding to the matching regions: labels or indices,
#'   depending on the value of `labels`.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
matchRegions <- function (regions, parcellation, labels = FALSE)
{
    if (!is.character(regions) && !is.integer(regions))
        report(OL$Error, "Regions must be specified in a character or integer vector")
    if (!is.list(parcellation) || !all(c("image","regions") %in% names(parcellation)))
        report(OL$Error, "The specified parcellation does not seem to be valid")
    
    findRegion <- function (name)
    {
        if (name %~% ore(integer))
            return (parcellation$regions$index == as.integer(name))
        else
        {
            haystack <- as.matrix(parcellation$regions[,c("label","lobe","type","hemisphere")])
            match <- (haystack == name)
            matchCounts <- colSums(match, na.rm=TRUE)
            if (all(matchCounts == 0))
                report(OL$Error, "Region specification \"#{name}\" does not match the parcellation lookup table")
            colToUse <- which(matchCounts > 0)[1]
            return (match[,colToUse])
        }
    }
    
    matches <- rep(FALSE, nrow(parcellation$regions))
    for (region in regions)
        matches <- matches | findRegion(region)
    matches <- which(matches)
    
    if (labels)
        return (parcellation$regions$label[matches])
    else
        return (parcellation$regions$index[matches])
}

#' Combine and resolve region labels
#' 
#' This function is a session-aware means of combining standard parcellations
#' with additional images to produce a hybrid labelled image for further use.
#' 
#' @param regions Named parcellation labels, and/or image file paths.
#' @param session An `MriSession` object representing the session of interest.
#' @param space The space in which the combined parcellation is required.
#'   Implicit co-registration will be performed and the structural space
#'   parcellation transformed in, if necessary. Some kind of ultimate source
#'   parcellation must exist unless all `regions` are file names.
#' @param parcellationConfidence Threshold level, ultimately passed to
#'   [transformParcellation()] if a transformation is required.
#' @return A list with elements
#'   - `image`: an integer-valued image representing the combined parcellation;
#'   - `indices`: the index values of the requested regions of interest, which
#'     may not match the source images if there is a label clash;
#'   - `labels`: the extracted region labels, which are based on the file name
#'     where they don't come from a standard parcellation; and
#'   - `fromParcellation`: a logical vector indicating which `indices` and
#'     `labels` come from a standard parcellation.
#' @seealso If only matching against a specific parcellation is needed,
#'   [matchRegions()] can be used instead.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#' 
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
resolveRegions <- function (regions, session, space = "structural", parcellationConfidence = 0.5)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    image <- session$getRegistrationTarget(space,metadataOnly=TRUE)$fill(0L)
    indices <- labels <- NULL
    areFiles <- imageFileExists(regions)
    
    if (any(!areFiles))
    {
        parcellation <- session$getParcellation(space, threshold=parcellationConfidence)
        indices <- sort(matchRegions(regions[!areFiles], parcellation))
        labels <- parcellation$regions$label[parcellation$regions$index %in% indices]
        locs <- which(parcellation$image$getData() %in% indices, arr.ind=TRUE)
        image[locs] <- parcellation$image[locs]
    }
    
    for (region in regions[areFiles])
    {
        # This makes "data" a SparseArray object
        currentImage <- readImageFile(region, sparse=TRUE)
        data <- currentImage$getData()
        positive <- (data$getData() > 0)
        locs <- data$getCoordinates()[positive,,drop=FALSE]
        currentIndices <- sort(unique(data$getData()[positive]))
        
        if (length(currentIndices) == 0)
            next
        
        if (!all(currentIndices == round(currentIndices)))
            report(OL$Error, "ROI image must be integer-valued")
        
        if (any(currentIndices %in% indices))
            delta <- max(indices)
        else
            delta <- 0L
        
        image[locs] <- data[locs] + delta
        indices <- c(indices, as.integer(currentIndices + delta))
        
        if (length(currentIndices) == 1)
            labels <- c(labels, basename(currentImage$getSource()))
        else
            labels <- c(labels, paste(basename(currentImage$getSource()),currentIndices,sep="_"))
    }
    
    return (list(image=image, indices=indices, labels=labels, fromParcellation=!areFiles))
}
