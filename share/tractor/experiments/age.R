#@args [DICOM or tagged NIfTI file]
#@desc Calculate the age of a scan subject to the day, using information from a DICOM file, or from a NIfTI file with suitable tag metadata. If no file is specified, the script will search the working directory for DICOM files, and use the first one it finds. BreakdownLevel should be set to 1 for years only (given as a real number); 2 for years and days; and 3 for years, months and days. BreakdownLevel is fixed to 1 if NumberOnly:true is given.
#@nohistory TRUE

library(tractor.base)

runExperiment <- function ()
{
    monthDays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    studyDate <- dateOfBirth <- NA
    
    if (nArguments() > 0)
        fileName <- Arguments[1]
    else
    {
        fileName <- NULL
        files <- expandFileName(list.files(".", full.names=TRUE, recursive=TRUE))
        files <- files[!file.info(files)$isdir]
        
        for (file in files)
        {
            metadata <- readDicomFile(file, ascii=FALSE)
            if (!is.null(metadata))
            {
                fileName <- file
                break
            }
        }
        
        if (is.null(fileName))
            report(OL$Error, "No DICOM files found under the current path")
    }
    
    breakDownLevel <- getConfigVariable("BreakdownLevel", 1, "integer")
    numberOnly <- getConfigVariable("NumberOnly", FALSE)
    
    if (breakDownLevel < 1 || breakDownLevel > 3)
        report(OL$Error, "BreakdownLevel must be between 1 and 3")
    
    report(OL$Info, "Reading date information from file \"", basename(fileName), "\"")
    metadata <- readDicomFile(fileName, ascii=FALSE)
    
    if (is.null(metadata))
    {
        image <- readImageFile(fileName)
        if (all(image$hasTags(c("studyDate", "patientBirthDate"))))
        {
            studyDate <- as.Date(image$getTags("studyDate"))
            dateOfBirth <- as.Date(image$getTags("patientBirthDate"))
        }
        else if (image$hasTags("patientAge"))
        {
            report(OL$Warning, "Patient age rather that date tags are available - the estimate will be limited in accuracy")
            age <- as.numeric(ore.subst("Y$", "", image$getTags("patientAge")))
            studyDate <- Sys.Date()
            dateOfBirth <- studyDate - (age * 365.2425)
        }
    }
    else
    {
        studyDate <- as.Date(metadata$getTagValue(0x0008,0x0020), "%Y%m%d")
        dateOfBirth <- as.Date(metadata$getTagValue(0x0010,0x0030), "%Y%m%d")
    }
    
    if (is.na(studyDate) || is.na(dateOfBirth))
        report(OL$Error, "Cannot read study date and birth date tags")
    
    report(OL$Info, "Study date is ", format(studyDate,"%Y-%m-%d"), " (yyyy-mm-dd)")
    report(OL$Info, "Patient's birth date is ", format(dateOfBirth,"%Y-%m-%d"))
    
    studyYear <- as.integer(format(studyDate, "%Y"))
    candidateYearsForLastBirthdate <- c(studyYear, studyYear-1)
    candidateLastBirthdates <- as.Date(paste(candidateYearsForLastBirthdate, format(dateOfBirth,"%m%d"), sep=""), "%Y%m%d")
    lastBirthdate <- which(as.numeric(studyDate-candidateLastBirthdates) >= 0)
    lastBirthdate <- candidateLastBirthdates[lastBirthdate[1]]
    
    birthMonth <- as.integer(format(dateOfBirth, "%m"))
    if (birthMonth != 1)
        monthDays <- monthDays[c(birthMonth:12, 1:(birthMonth-1))]
    
    years <- as.numeric(lastBirthdate - dateOfBirth) %/% 365.2425
    remainingDays <- as.numeric(studyDate - lastBirthdate)
    fractionalYears <- years + (remainingDays / 365.2425)
    
    partialRemnants <- remainingDays - cumsum(monthDays)
    months <- which(partialRemnants < monthDays & partialRemnants >= 0)
    if (length(months) >= 1)
    {
        months <- months[1]
        days <- partialRemnants[months]
    }
    else
    {
        months <- 0
        days <- remainingDays
    }
    
    if (numberOnly)
        cat(paste(round(fractionalYears,3), "\n", sep=""))
    else
    {
        outputString <- switch(breakDownLevel,
                               paste(round(fractionalYears,3), " year(s)", sep=""),
                               paste(years, " year(s) and ", remainingDays, " day(s)", sep=""),
                               paste(years, " year(s), ", months, " month(s) and ", days, " day(s)", sep=""))
        report(OL$Info, "Age at study date is ", outputString)
    }
}
