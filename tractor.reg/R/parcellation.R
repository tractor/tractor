readParcellation <- function (imageFileName, regionFileName = NULL, ...)
{
    image <- readImageFile(imageFileName, ...)
    if (is.null(regionFileName))
        regionFileName <- ensureFileSuffix(image$getSource(), "lut")
    regions <- read.table(regionFileName, header=TRUE, stringsAsFactors=FALSE)
    return (list(image=image, regions=regions))
}

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
