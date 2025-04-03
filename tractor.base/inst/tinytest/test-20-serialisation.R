reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

path <- system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base")
image <- readImageFile(path)
file <- tempfile()

image$serialise(file)
raw <- deserialiseReferenceObject(file, raw=TRUE)
object <- deserialiseReferenceObject(file)

expect_equal(serialiseReferenceObject(image), raw)
expect_true(isDeserialisable(raw))
expect_inherits(raw, "list")
expect_false(is.nilObject(object))
expect_inherits(object, "MriImage")
