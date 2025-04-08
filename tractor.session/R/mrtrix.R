showImagesInMrview <- function (imageFileNames, wait = FALSE, lookupTable = NULL, opacity = NULL)
{
    # Every image except the first is considered an overlay so its options need a prefix
    prefixes <- c("", rep("overlay.",length(imageFileNames)-1))
    
    # Add directives to load the images and disable interpolation
    imageFileNames <- es("-#{prefixes}load #{shQuote(imageFileNames)} -#{prefixes}interpolation 0")
    
    if (!is.null(lookupTable))
    {
        lookupTable <- rep(lookupTable, length.out=length(imageFileNames))
        validColourMaps <- c("Gray", "Hot", "Cool", "Jet", "Inferno", "Viridis", "PET")
        
        # MRView has an indexing quirk where the main image colour maps are 1-based but overlays are 0-based
        indices <- match(lookupTable, validColourMaps) - c(0, rep(1,length(imageFileNames)-1))
        if (any(is.na(indices)))
            report(OL$Warning, "Lookup table name(s) ", implode(unique(lookupTable[is.na(indices)]),sep=", ",finalSep=" and "), " are not valid for MRView")
        
        imageFileNames <- es("#{imageFileNames} -#{prefixes}colourmap #{indices}")
    }
    if (!is.null(opacity) && length(imageFileNames) > 1)
    {
        opacity <- rep(opacity, length.out=length(imageFileNames))
        imageFileNames[-1] <- es("#{imageFileNames[-1]} -overlay.opacity #{opacity[-1]}")
    }
    
    execute("mrview", implode(imageFileNames,sep=" "), errorOnFail=TRUE, wait=wait, silent=TRUE)
    
    invisible(unlist(imageFileNames))
}
