setClassUnion("MriImageOrNull", c("MriImage","NULL"))

Registration <- setRefClass("Registration", contains="SerialisableObject", fields=list(sourceMetadata="MriImageMetadata",targetMetadata="MriImageMetadata",finalImage="MriImage",reverseImage="MriImageOrNull",affineMatrices="list",controlPointImages="list",reverseControlPointImages="list"), methods=list(
    getAffineMatrices = function (i = NULL)
    {
        if (is.null(i))
            return (affineMatrices)
        else
            return (affineMatrices[i])
    },
))
