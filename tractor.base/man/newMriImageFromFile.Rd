\name{newMriImageFromFile}
\alias{newMriImageFromFile}
\alias{newMriImageMetadataFromFile}
\alias{writeMriImageToFile}
\alias{identifyImageFileNames}
\alias{imageFileExists}
\alias{removeImageFilesWithName}
\title{Working with MRI images stored in NIfTI and Analyze formats}
\description{
  Functions for reading, writing, locating and removing MRI images stored in NIfTI and Analyze formats.
}
\usage{
newMriImageFromFile(fileName, fileType = NULL)
newMriImageMetadataFromFile(fileName, fileType = NULL)
writeMriImageToFile(image, fileName = NULL, fileType = NA, datatype = NULL, overwrite = TRUE)

identifyImageFileNames(fileName, fileType = NULL, errorIfMissing = TRUE)
imageFileExists(fileName, fileType = NULL)
removeImageFilesWithName(fileName)
}
\arguments{
  \item{fileName}{A file name, with or without appropriate extension.}
  \item{image}{An MriImage object.}
  \item{fileType}{A character vector of length one, giving the file type required or expected. If this option is missing, the file type used for writing images will be taken from the \code{tractorFileType} option. See Details.}
  \item{datatype}{A storage data type. See \code{\link{getDataTypeByNiftiCode}}.}
  \item{overwrite}{Logical value: overwrite an existing image file? An error will be raised if there is an existing file and this is set to FALSE.}
  \item{errorIfMissing}{Logical value: raise an error if no suitable files were found?}
}
\details{
  NIfTI and Analyze are related formats for storing magnetic resonance images. NIfTI is a more recent extension of Analyze, and contains more specific information about, for example, the orientation of the image. Its use is therefore recommended where possible. These formats use a number of different file extensions, but the details are abstracted away from the user by these functions.
  
  TractoR does not allow for files with the same basic name using multiple Analyze/NIfTI formats in a single directory (e.g. "foo.nii" AND "foo.img"), and these functions will produce an error if multiple compatible files exist.

  Suitable values for \code{fileType} (and the \code{tractorFileType} option, which is used as a default) are \code{ANALYZE}, \code{NIFTI}, \code{NIFTI_PAIR} (the two-file NIfTI format), \code{ANALYZE_GZ}, \code{NIFTI_GZ} and \code{NIFTI_PAIR_GZ}. The latter three are gzipped versions of the former three. \code{NIFTI_GZ} is recommended unless there is a need for one of the others. This is the default value for the \code{tractorFileType} option, but that can be changed using a call to \code{\link{options}}, or by setting the \code{TRACTOR_FILETYPE} environment variable before loading the tractor.base package.
}
\value{
  \code{newMriImageFromFile} and \code{newMriImageMetadataFromFile} return the appropriate image or metadata object. \code{imageFileExists} returns TRUE if an existing file with the specified name exists (all file extensions are checked), and FALSE otherwise. \code{removeImageFilesWithName} returns the result of \code{\link{unlink}}() applied to all relevant files. \code{writeMriImageToFile} and \code{identifyImageFileNames} return a list with the following elements, describing the identified or written files:
  \item{fileStem}{The file name without extension.}
  \item{headerFile}{The full header file name.}
  \item{imageFile}{The full image file name.}
  \item{format}{The format of the files ("Nifti" or "Analyze"). Not returned by \code{writeMriImageToFile}.}
}
\author{Jon Clayden}
\seealso{The NIfTI-1 standard (\url{http://nifti.nimh.nih.gov/nifti-1}), \code{\link{MriImage}}, \code{\link{MriImageMetadata}}, \code{\link{getDataTypeByNiftiCode}} and \code{\link{unlink}}.}
