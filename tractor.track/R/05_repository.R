StreamlineRepository <- setRefClass("StreamlineRepository", fields=list(fileName="character"), methods=list(
    initialize = function (fileName = NULL, ...)
    {
        if (is.null(fileName))
            fileName <- threadSafeTempFile("strepo")
        fileName <- ensureFileSuffix(expandFileName(fileName), "trk")
        return (initFields(fileName=fileName))
    },
    
    getFileName = function () { return (fileName) }
))


