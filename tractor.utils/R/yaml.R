readYaml <- function (fileNames)
{
    # Handle multiple file names separated by colons
    fileNames <- unlist(ore.split(ore(":",syntax="fixed"), fileNames, simplify=FALSE)) %~|% "\\S"
    
    result <- NULL
    for (fileName in fileNames)
    {
        text <- readLines(fileName, encoding="UTF-8")
        text <- ore.subst("^(\\s*[\\w-]+\\s*):(?! )", "\\1: ", text)
        result <- c(result, yaml::yaml.load(paste(text, collapse="\n")))
    }
    
    return (deduplicate(result))
}

writeYaml <- function (object, fileName)
{
    text <- yaml::as.yaml(object)
    Encoding(text) <- "UTF-8"
    writeLines(text, fileName)
}
