reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

# Internal viewer
image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
expect_null(viewImages(image, interactive=FALSE))

# Static visualisations
image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
fov <- image$getFieldOfView()
file <- ensureFileSuffix(tempfile(), "png")

if (requireNamespace("loder", quietly=TRUE))
{
    createSliceGraphic(image, z=30, device="png", file=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1])
    createProjectionGraphic(image, 3, device="png", file=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1])
    createContactSheetGraphic(image, 3, device="png", file=file, nColumns=8)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1]*8)
    tractor.base:::compositeImages(list(image), x=48, y=48, z=30, prefix=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1]*2)
}
