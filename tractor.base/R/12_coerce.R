setOldClass(c("niftiImage", "internalImage"))

.convertNiftiImage <- function (from)
{
    # Pick up divest attributes and convert to tags (anonymising by default)
    attribs <- attributes(from)
    anonymise <- attr(from, "anonymise") %||% TRUE
    attribs <- attribs[!(names(attribs) %~% "^\\.|^(dim|imagedim|pixdim|pixunits|class|reordered|anonymise)$")]
    if (anonymise)
        attribs <- attribs[!(names(attribs) %~% "^patient")]
    
    if (length(attribs) > 0)
        tags <- attribs
    else
        tags <- list()
    
    if (RNifti:::hasData(from))
        data <- as.array(from)
    else
        data <- NULL
    
    metadata <- niftiHeader(from)
    defaults <- list(dim_info=0, intent_p1=0, intent_p2=0, intent_p3=0, intent_code=0, intent_name="", slice_start=0, slice_end=0, slice_code=0, cal_min=0, cal_max=0, slice_duration=0, toffset=0, aux_file="")
    
    # Add NIfTI attributes that are set to something other than the defaults
    for (key in names(defaults))
    {
        if (metadata[[key]] != defaults[[key]])
            tags[[key]] <- metadata[[key]]
    }
    
    reordered <- attr(from, "reordered") %||% FALSE
    
    return (MriImage$new(imageDims=dim(from), pixdim(from), voxelDimUnits=pixunits(from), origin=origin(from), xform=xform(from), reordered=reordered, tags=tags, data=unclass(data)))
}

setAs("niftiImage", "MriImage", .convertNiftiImage)
setAs("internalImage", "MriImage", .convertNiftiImage)

setAs("MriImage", "nifti", function(from) {
    if (is.null(getOption("niftiAuditTrail")))
        options(niftiAuditTrail=FALSE)
    
    nifti <- as.array(retrieveNifti(from))
    if (RNifti:::hasData(nifti))
        nifti <- updateNifti(nifti, list(cal_min=min(nifti,na.rm=TRUE), cal_max=max(nifti,na.rm=TRUE)))
    
    return (oro.nifti::nii2oro(nifti))
})

setAs("nifti", "MriImage", function(from) as(retrieveNifti(from), "MriImage"))

# MriImage methods for RNifti generics
#' @export
pixdim.MriImage <- function (object)
{
    object$getVoxelDimensions()
}

#' @export
"pixdim<-.MriImage" <- function (object, value)
{
    report(OL$Error, "MriImage voxel dimensions cannot be changed at present")
}

#' @export
pixunits.MriImage <- function (object)
{
    object$getVoxelUnits()
}

#' @export
"pixunits<-.MriImage" <- function (object, value)
{
    report(OL$Error, "MriImage voxel units cannot be changed at present")
}

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
    
    return (jsonlite::toJSON(bids, digits=NA, pretty=TRUE))
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
