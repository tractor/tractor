context("Visualisation")

reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

test_that("we can use the internal viewer", {
    image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
    expect_null(viewImages(image, interactive=FALSE))
    grDevices::dev.off()
})

test_that("we can create static visualisations", {
    image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
    fov <- image$getFieldOfView()
    file <- ensureFileSuffix(tempfile(), "png")
    
    skip_if_not_installed("loder", "0.2.0")
    
    createSliceGraphic(image, z=30, device="png", file=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1])
    createProjectionGraphic(image, 3, device="png", file=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1])
    createContactSheetGraphic(image, 3, device="png", file=file, nColumns=8)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1]*8)
    tractor.base:::compositeImages(list(image), x=48, y=48, z=30, prefix=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1]*2)
})
