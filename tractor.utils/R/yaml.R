#' Read and write configuration variables in YAML format
#'
#' These functions read and write TractoR configuration variables using the
#' YAML format, via the `yaml` package. `readYaml()` is tolerant of a
#' missing space after the colon in `key:value` pairs, which is a common
#' shorthand used when writing configuration on the command line.
#'
#' @param fileNames A character vector, or single colon-separated string,
#'   of paths to YAML files. Elements which are empty or contain no
#'   non-whitespace characters are ignored. If more than one file is given,
#'   the resulting lists are merged, with later files' values taking
#'   priority for duplicate keys (see `deduplicate()`).
#' @param object A list of configuration variables to write out.
#' @param fileName A string giving the path to write to.
#' @return For `readYaml()`, a list of configuration variables read from
#'   the file(s). `writeYaml()` is called for its side effect.
#' @author Jon Clayden
#' @rdname yaml
#' @export
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

#' @rdname yaml
#' @export
writeYaml <- function (object, fileName)
{
    text <- yaml::as.yaml(object)
    Encoding(text) <- "UTF-8"
    writeLines(text, fileName)
}
