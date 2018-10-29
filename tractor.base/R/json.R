convertTagsToJson <- function (tags)
{
    if (is(tags, "MriImage"))
        tags <- tags$getTags()
    bids <- list()
    
    if (all(c("phaseEncodingDirection","phaseEncodingSign") %in% names(tags)))
    {
        bids$PhaseEncodingDirection <- es("#{tags$phaseEncodingDirection}#{ifelse(tags$phaseEncodingSign < 0,'-','')}")
        tags <- tags[setdiff(names(tags), c("phaseEncodingDirection","phaseEncodingSign"))]
    }
    
    for (tagName in names(tags))
    {
        if (tagName %~% .Bids$toIgnore)
            next
        
        value <- tags[[tagName]]
        
        # Tag names mostly reflect the BIDS ones, but with the first letter downcased
        # A few exceptions are mapped explicitly
        if (tagName %in% names(.Bids$mappingToJson))
            bidsName <- .Bids$mappingToJson[[tagName]]
        else
            bidsName <- paste0(toupper(substring(tagName,1,1)), substring(tagName,2))
        
        # BIDS always uses seconds for time fields, but we use milliseconds in some places
        if (bidsName %~% .Bids$toScale)
            value <- value / 1e3
        bids[[bidsName]] <- value
    }
    
    return (jsonlite::toJSON(bids, auto_unbox=TRUE, digits=NA, pretty=TRUE))
}

convertJsonToTags <- function (bids)
{
    bids <- jsonlite::fromJSON(bids, simplifyVector=TRUE)
    tags <- list()
    for (bidsName in names(bids))
    {
        if (bidsName %~% .Bids$toIgnore)
            next
        
        value <- bids[[bidsName]]
        if (bidsName == "PhaseEncodingDirection")
        {
            groups <- ore::groups(ore.search("^([ijk])(-)?$", value))
            tags$phaseEncodingDirection <- groups[,1]
            tags$phaseEncodingSign <- ifelse(is.na(groups[,2]), 1L, -1L)
        }
        
        # Tag names mostly reflect the BIDS ones, but with the first letter downcased
        # A few exceptions are mapped explicitly
        if (bidsName %in% names(.Bids$mappingFromJson))
            tagName <- .Bids$mappingFromJson[[bidsName]]
        else
            tagName <- paste0(tolower(substring(bidsName,1,1)), substring(bidsName,2))
        
        # BIDS always uses seconds for time fields, but we use milliseconds in some places
        if (bidsName %~% .Bids$toScale)
            value <- value * 1e3
        tags[[tagName]] <- value
    }
    
    return (tags)
}
