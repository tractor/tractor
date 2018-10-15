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
    merge <- getConfigVariable("Merge", FALSE)
    
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
    
    copyAndConvert <- function (from, to)
    {
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
                from <- tempfile()
                writeImageFile(info$image, from, writeTags=TRUE)
            }
        }
        
        info <- identifyImageFiles(from, auxiliaries=c("bval","bvec","dirs","json","tags"), errorIfMissing=FALSE)
        if (is.null(info))
        {
            report(OL$Warning, "Skipping #{from} (source image not found)")
            return (NULL)
        }
        
        imageIndex <- function (i)
        {
            if (from %~% "_(bold|dwi)$")
                ore.subst("_(\\w+)$", es("_acq-#{i}_\\1"), info$imageStem)
            else
                es("#{info$imageStem}_acq-#{i}")
        }
        
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
        
        dir <- dirname(to)
        if (!file.exists(dir))
            dir.create(dir, recursive=TRUE)
        
        if (info$format != "Nifti")
        {
            report(OL$Warning, "Image #{from} is not in NIfTI format, so needs to be converted")
            image <- readImageFile(from, reorder=FALSE)
            writeImageFile(image, to, "NIFTI_GZ", writeTags=FALSE)
        }
        
        
    }
    
    if (!file.exists(sessionPath))
        dir.create(sessionPath, recursive=TRUE)
    
    fileStem <- es("sub-#{subjectId}")
    if (!is.null(sessionId))
        fileStem <- paste(fileStem, es("ses-#{sessionId}"), sep="_")
    
    tasks <- rep(tasks, length.out=length(functional))
    
    for (imagePath in t1w)
        copyAndConvert(imagePath, file.path(sessionPath, "anat", es("#{fileStem}_T1w")))
    for (imagePath in t2w)
        copyAndConvert(imagePath, file.path(sessionPath, "anat", es("#{fileStem}_T2w")))
    for (imagePath in pdw)
        copyAndConvert(imagePath, file.path(sessionPath, "anat", es("#{fileStem}_PD")))
    for (i in seq_along(functional))
        copyAndConvert(functional[i], file.path(sessionPath, "func", es("#{fileStem}_task-#{tasks[i]}_bold")))
    for (imagePath in diffusion)
        copyAndConvert(imagePath, file.path(sessionPath, "dwi", ))
}
