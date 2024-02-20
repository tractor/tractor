reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

path <- system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base")

# RNifti is a hard dependency, so we don't have to check if it's installed
image <- readImageFile(path)
expect_equal(RNifti::pixdim(image), rep(2.5,3L))
expect_equal(RNifti::pixunits(image), c(spatial="mm",temporal="s"))

image <- RNifti::retrieveNifti(image)
expect_equal(RNifti::pixdim(image), rep(2.5,3L))
expect_equal(RNifti::pixunits(image), c("mm","s"))

image <- as(RNifti::readNifti(path,internal=FALSE), "MriImage")
expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
expect_equal(image$getVoxelUnits(), c(spatial="mm",temporal="s"))

image <- as(RNifti::readNifti(path,internal=TRUE), "MriImage")
expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
expect_equal(image$getVoxelUnits(), c(spatial="mm",temporal="s"))

# But oro.nifti might be missing
if (requireNamespace("oro.nifti", quietly=TRUE))
{
    # Suppress messages about double-declaration of the internalImage class
    suppressMessages({
        path <- system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base")
        
        image <- readImageFile(path)
        expect_true(validObject(as(image, "nifti")))
        
        image <- suppressWarnings(as(oro.nifti::readNIfTI(path), "MriImage"))
        expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
        expect_equal(image$getVoxelUnits(), c(spatial="mm",temporal="s"))
    })
}
