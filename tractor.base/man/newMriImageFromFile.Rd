\name{newMriImageFromFile}
\alias{newMriImageFromFile}
\alias{newMriImageMetadataFromFile}
\alias{writeMriImageToFile}
\alias{identifyImageFileNames}
\alias{imageFileExists}
\alias{removeImageFilesWithName}
\alias{copyImageFiles}
\alias{symlinkImageFiles}
\title{Working with MRI images stored in NIfTI, Analyze and MGH formats}
\description{
Functions for reading, writing, locating, copying and removing MRI images stored in NIfTI, Analyze and MGH formats.
}
\usage{
newMriImageFromFile(fileName, fileType = NULL, volumes = NULL, sparse = FALSE, mask = NULL)
newMriImageMetadataFromFile(fileName, fileType = NULL)
writeMriImageToFile(image, fileName = NULL, fileType = NA, datatype = NULL, overwrite = TRUE)

identifyImageFileNames(fileName, fileType = NULL, errorIfMissing = TRUE)
imageFileExists(fileName, fileType = NULL)
removeImageFilesWithName(fileName)
copyImageFiles(from, to, overwrite = FALSE, deleteOriginals = FALSE)
symlinkImageFiles(from, to, overwrite = FALSE, relative = TRUE)
}
\arguments{
  \item{fileName, from, to}{File names, with or without appropriate extension.}
  \item{image}{An \code{\linkS4class{MriImage}} object.}
  \item{fileType}{A character vector of length one, giving the file type required or expected. If this option is missing, the file type used for writing images will be taken from the \code{tractorFileType} option. See Details.}
  \item{volumes}{An optional integer vector specifying a subset of volumes to read (generally to save memory). If given, only the requested volumes in the 4D file will be read.}
  \item{sparse}{Logical value: should the image data be stored in a \code{\linkS4class{SparseArray}} object?}
  \item{mask}{An optional \code{\linkS4class{MriImage}} object representing a mask, outside of which the image to be read should be considered to be zero. This can be used to save memory when only a small part of a large image is of interest. Ignored if \code{sparse} is not \code{TRUE}.}
  \item{datatype}{A storage data type. See \code{\link{getDataTypeByNiftiCode}}.}
  \item{overwrite}{Logical value: overwrite an existing image file? For \code{writeMriImageToFile}, an error will be raised if there is an existing file and this is set to FALSE.}
  \item{errorIfMissing}{Logical value: raise an error if no suitable files were found?}
  \item{deleteOriginals}{Logical value: if \code{TRUE}, \code{copyImageFiles} performs a move rather than a copy.}
  \item{relative}{Logical value: if \code{TRUE}, the path stored in the symlink will be relative (e.g. \code{"../some_dir/some_image.nii"}) rather than absolute (e.g. \code{"/path/to/some_dir/some_image.nii"}).}
}
\details{
  NIfTI and Analyze are related formats for storing magnetic resonance images. NIfTI is a more recent extension of Analyze, and contains more specific information about, for example, the orientation of the image. Its use is therefore recommended where possible. MGH format is used by the popular image processing package FreeSurfer. These formats use a number of different file extensions, but the details are abstracted away from the user by these functions.
  
  TractoR does not allow for files with the same basic name using multiple Analyze/NIfTI/MGH formats in a single directory (e.g. \code{"foo.nii"} AND \code{"foo.img"}), and these functions will produce an error if multiple compatible files exist.

  Suitable values for \code{fileType} (and the \code{tractorFileType} option, which is used as a default) are \code{ANALYZE}, \code{NIFTI}, \code{NIFTI_PAIR} (the two-file NIfTI format), \code{MGH}, \code{ANALYZE_GZ}, \code{NIFTI_GZ}, \code{NIFTI_PAIR_GZ} and \code{MGH_GZ}. The latter four are gzipped versions of the former four. \code{NIFTI_GZ} is recommended unless there is a need for one of the others. This is the default value for the \code{tractorFileType} option, but that can be changed using a call to \code{\link{options}}, or by setting the \code{TRACTOR_FILETYPE} environment variable before loading the tractor.base package.
  
  Since multiple files may be involved, copying, moving or symlinking images is not trivial. \code{copyImageFiles} and \code{symlinkImageFiles} are wrappers around the standard functions \code{\link{file.copy}} and \code{\link{file.symlink}} which handle this complexity.
}
\value{
  \code{newMriImageFromFile} and \code{newMriImageMetadataFromFile} return the appropriate image or metadata object. \code{imageFileExists} returns \code{TRUE} if an existing file with the specified name exists (all file extensions are checked), and \code{FALSE} otherwise. \code{removeImageFilesWithName} returns the result of \code{\link{unlink}} applied to all relevant files. \code{writeMriImageToFile} and \code{identifyImageFileNames} return a list with the following elements, describing the identified or written files:
  \item{fileStem}{The file name without extension.}
  \item{headerFile}{The full header file name.}
  \item{imageFile}{The full image file name.}
  \item{format}{The format of the files (\code{"Nifti"}, \code{"Analyze"} or \code{"Mgh"}). Not returned by \code{writeMriImageToFile}.}
  \code{copyImageFiles} and \code{symlinkImageFiles} are called for their side effects.
}
\author{Jon Clayden}
\references{
Please cite the following reference when using TractoR in your work:

J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A. Clark (2011). TractoR: Magnetic resonance imaging and tractography with R. Journal of Statistical Software 44(8):1-18. \url{http://www.jstatsoft.org/v44/i08/}.
}
\seealso{The NIfTI-1 standard (\url{http://nifti.nimh.nih.gov/nifti-1}); \code{\linkS4class{MriImage}}, \code{\linkS4class{MriImageMetadata}}, \code{\link{getDataTypeByNiftiCode}} and \code{\link{unlink}}.}
