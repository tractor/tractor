library(jsonlite)

runExperiment <- function ()
{
    sessionPath <- expandFileName(ifelse(nArguments()==0, ".", Arguments[1]))
    
    t1w <- getConfigVariable("T1w", NULL, "character", multiple=TRUE)
    t2w <- getConfigVariable("T2w", NULL, "character", multiple=TRUE)
    pdw <- getConfigVariable("PDw", NULL, "character", multiple=TRUE)
    functional <- getConfigVariable("Functional", NULL, "character", multiple=TRUE)
    tasks <- getConfigVariable("FunctionalTasks", "rest", "character", multiple=TRUE)
    diffusion <- getConfigVariable("Diffusion", NULL, "character", multiple=TRUE)
    patternMatch <- getConfigVariable("PatternMatch", FALSE)
    
    subjectId <- sessionId <- NULL
    if (sessionPath %~% "/sub-(\\w+)(/ses-(\\w+))?/?")
    {
        groups <- ore::groups(ore.lastmatch())
        subjectId <- groups[1]
        if (!is.na(groups[3]))
            sessionId <- groups[3]
    }
    else
        report(OL$Error, "The specified session path does not seem to fit the BIDS naming convention")
    
    matchSources <- function (paths)
    {
        if (!patternMatch)
            return (paths)
        else
        {
            suffixPattern <- implode(unique(tractor.base:::.FileTypes$imageSuffixes), "|")
            return (unlist(lapply(paths, function(path) {
                pattern <- ore(ore.escape(basename(path)), ".+(", suffixPattern, ")$")
                list.files(dirname(path), full.names=TRUE) %~|% pattern
            })))
        }
    }
    
    copyAndConvert <- function (from, to)
    {
        # If the source is a directory, assume it contains DICOM files
        if (isTRUE(file.info(from)$isdir))
        {
            report(OL$Info, "#{from} is a directory - searching for DICOM files")
            info <- readDicomDirectory(from, method="divest", interactive=FALSE)
            if (is.null(info))
            {
                report(OL$Warning, "Skipping #{from} (unable to convert from DICOM)")
                return (NULL)
            }
            else
            {
                # Write converted image to a temporary file to simplify the logic
                from <- tempfile()
                writeImageFile(info$image, from, writeTags=TRUE)
            }
        }
        
        # Check whether the source image is present (and with which auxiliaries)
        info <- identifyImageFileNames(from, auxiliaries=c("bval","bvec","dirs","json","tags"), errorIfMissing=FALSE)
        if (is.null(info))
        {
            report(OL$Warning, "Skipping #{from} (source image not found)")
            return (NULL)
        }
        
        imageIndex <- function (i)
        {
            if (to %~% "_(bold|dwi)$")
                ore.subst("_(\\w+)$", es("_acq-#{i}_\\1"), to)
            else
                es("#{to}_acq-#{i}")
        }
        
        # Handle multiple files with the same target name by adding to the "acq" label
        if (imageFileExists(imageIndex(1L)))
        {
            i <- 2L
            while (imageFileExists(imageIndex(i)))
                i <- i + 1L
            to <- imageIndex(i)
        }
        else if (imageFileExists(to))
        {
            copyImageFiles(to, imageIndex(1L), deleteOriginals=TRUE, auxiliaries=c("bval","bvec","json"))
            to <- imageIndex(2L)
        }
        
        # Create the target directory if necessary
        dir <- dirname(to)
        if (!file.exists(dir))
            dir.create(dir, recursive=TRUE)
        
        # If there's a source JSON file, copy it directly...
        if ("json" %in% info$auxiliarySuffixes)
            copyImageFiles(from, to, auxiliaries=c("bval","bvec","json"))
        else
        {
            # Otherwise read tag metadata and produce .bval, .bvec and .json files as needed
            # No anonymisation is done here - whatever's in the source files will be kept
            copyImageFiles(from, to, auxiliaries=NULL)
            tags <- readImageFile(from, metadataOnly=TRUE)$getTags()
            if ("bValues" %in% names(tags))
                write.table(promote(tags$bValues,byrow=TRUE), ensureFileSuffix(to,"bval"), row.names=FALSE, col.names=FALSE)
            if ("bVectors" %in% names(tags))
                write.table(t(tags$bVectors), ensureFileSuffix(to,"bvec"), row.names=FALSE, col.names=FALSE)
            tags <- tags[!(names(tags) %~% "^(toffset|bValues|bVectors)$|_")]
            if (length(tags) > 0)
                writeLines(tractor.base:::convertTagsToJson(tags), ensureFileSuffix(to,"json"))
        }
        
        # If the source file was not in NIfTI format (which BIDS requires), read and rewrite it
        if (info$format != "Nifti")
        {
            report(OL$Warning, "Image #{from} is not in NIfTI format, so needs to be converted")
            image <- readImageFile(to, reorder=FALSE)
            writeImageFile(image, to, "NIFTI_GZ", writeTags=FALSE)
        }
    }
    
    if (!file.exists(sessionPath))
        dir.create(sessionPath, recursive=TRUE)
    
    fileStem <- es("sub-#{subjectId}")
    if (!is.null(sessionId))
        fileStem <- paste(fileStem, es("ses-#{sessionId}"), sep="_")
    
    functional <- matchSources(functional)
    tasks <- rep(tasks, length.out=length(functional))
    
    for (imagePath in matchSources(t1w))
        copyAndConvert(imagePath, file.path(sessionPath, "anat", es("#{fileStem}_T1w")))
    for (imagePath in matchSources(t2w))
        copyAndConvert(imagePath, file.path(sessionPath, "anat", es("#{fileStem}_T2w")))
    for (imagePath in matchSources(pdw))
        copyAndConvert(imagePath, file.path(sessionPath, "anat", es("#{fileStem}_PD")))
    for (i in seq_along(functional))
        copyAndConvert(functional[i], file.path(sessionPath, "func", es("#{fileStem}_task-#{tasks[i]}_bold")))
    for (imagePath in matchSources(diffusion))
        copyAndConvert(imagePath, file.path(sessionPath, "dwi", es("#{fileStem}_dwi")))
}
