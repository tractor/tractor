#' The dicomMetadata class (S7)
#'
#' This S7 class represents DICOM metadata, which typically contains detailed
#' information about the scan parameters and subject. This is a value-semantics
#' replacement for the \code{\linkS4class{DicomMetadata}} reference class,
#' which is now a thin backwards-compatible shell around this class. Unlike
#' the other classes ported in this release, no reference-semantics bug was
#' ever observed here (the original class has no mutating methods) - it is
#' included for consistency with the rest of the new API.
#'
#' @param source String naming the source file.
#' @param tags Data frame of tag information.
#' @param tagOffset Starting offset for tags in the file.
#' @param dataOffset Starting offset for pixel data in the file.
#' @param dataLength Pixel data length.
#' @param explicitTypes Logical value indicating whether explicit types are
#'   used in the file.
#' @param endian String naming the endianness of the file.
#' @param asciiFields Character vector containing the contents of the ASCII
#'   header, if requested and present in the file.
#' @param transferSyntax Transfer syntax string, if specified in the file;
#'   otherwise the empty string.
#'
#' @export
dicomMetadata <- S7::new_class("dicomMetadata", package = NULL, properties = list(
    source = S7::class_character,
    tags = S7::new_S3_class("data.frame"),
    tagOffset = S7::class_integer,
    dataOffset = S7::class_integer,
    dataLength = S7::class_integer,
    explicitTypes = S7::class_logical,
    endian = S7::class_character,
    asciiFields = S7::class_character,
    transferSyntax = S7::class_character))

S7::S4_register(dicomMetadata)

#' @export
print.dicomMetadata <- function (x, descriptions = FALSE, ...)
{
    .dicomMetadataPrint(x, descriptions=descriptions, ...)
    invisible(x)
}

#' Retrieve a DICOM tag value
#'
#' @param x A \code{dicomMetadata} object.
#' @param group,element Integer group and element numbers of the tag.
#' @export
tagValue <- S7::new_generic("tagValue", "x")

#' @export
method(tagValue, dicomMetadata) <- function (x, group, element)
{
    .dicomMetadataGetTagValue(x, group, element)
}

#' Retrieve fields from a DICOM ASCII header
#'
#' @param x A \code{dicomMetadata} object.
#' @param pattern An optional regular expression to match field names.
#' @export
asciiField <- S7::new_generic("asciiField", "x")

#' @export
method(asciiField, dicomMetadata) <- function (x, pattern = NULL)
{
    if (is.null(pattern))
        return (x@asciiFields)

    regex <- ore("^\\s*(\\S*", pattern, "\\S*)\\s*=\\s*(.+)\\s*$")
    groups <- groups(ore.search(regex, x@asciiFields, simplify=FALSE), simplify=TRUE)
    if (all(is.na(groups)))
        return (NA)

    values <- groups[,ncol(groups),]
    if (all(values %~% ore("^",number,"$")))
        values <- as.numeric(values)
    names(values) <- groups[,ncol(groups)-1,]
    return (values)
}

#' Retrieve the transfer syntax of a DICOM file
#'
#' @param x A \code{dicomMetadata} object.
#' @export
transferSyntax <- S7::new_generic("transferSyntax", "x")

#' @export
method(transferSyntax, dicomMetadata) <- function (x) x@transferSyntax

.dicomMetadataGetTagValue <- function (x, group, element)
{
    tags <- x@tags
    valueRow <- subset(tags, (tags$groups == group & tags$elements == element))
    if (dim(valueRow)[1] == 0 || valueRow$values == "")
        return (NA)
    else
    {
        value <- unlist(strsplit(as.vector(valueRow$values), "\\", fixed=TRUE, useBytes=TRUE))
        if (capabilities("iconv") == TRUE)
            value <- iconv(value, "", "LATIN1", sub="byte")
        value <- gsub("^\\s*(.+?)\\s*$", "\\1", value, perl=TRUE)

        if (as.vector(valueRow$types) %in% .Dicom$convertibleTypes)
            return (as.numeric(value))
        else
            return (value)
    }
}

.dicomMetadataPrint <- function (x, descriptions = FALSE, ...)
{
    tags <- x@tags
    nTags <- nrow(tags)

    if (nTags > 0)
    {
        if (descriptions)
        {
            cat("DESCRIPTION", rep(" ",19), "VALUE\n", sep="")
            for (i in seq_len(nTags))
            {
                description <- getDescriptionForDicomTag(tags$groups[i], tags$elements[i])
                cat(" ", substr(description, 1, 27), sep="")
                nSpaces <- max(3, 30-nchar(description))
                cat(rep(" ",nSpaces), sep="")
                cat(implode(.dicomMetadataGetTagValue(x,tags$groups[i],tags$elements[i]), sep=", "))
                cat("\n")
            }
        }
        else
        {
            cat("GROUP    ELEMENT  VALUE\n")
            for (i in seq_len(nTags))
            {
                cat(sprintf(" 0x%04x   0x%04x   ", tags$groups[i], tags$elements[i]))
                cat(implode(.dicomMetadataGetTagValue(x,tags$groups[i],tags$elements[i]), sep=", "))
                cat("\n")
            }
        }
    }
}
