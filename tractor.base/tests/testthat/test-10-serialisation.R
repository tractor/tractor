context("Serialisation")

reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

test_that("images can be serialised and deserialised", {
    path <- system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base")
    image <- readImageFile(path)
    file <- tempfile()
    
    image$serialise(file)
    raw <- deserialiseReferenceObject(file, raw=TRUE)
    object <- deserialiseReferenceObject(file)
    
    expect_equal(serialiseReferenceObject(image), raw)
    expect_true(isDeserialisable(raw))
    expect_type(raw, "list")
    expect_false(is.nilObject(object))
    expect_s4_class(object, "MriImage")
})
