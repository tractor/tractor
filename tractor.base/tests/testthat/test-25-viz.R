context("Visualisation")

reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

test_that("we can use the internal viewer", {
    image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
    expect_null(viewImages(image, interactive=FALSE))
    grDevices::dev.off()
})
