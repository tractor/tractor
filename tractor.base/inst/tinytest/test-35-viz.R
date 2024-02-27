reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal, mmand.threads=1L)

# Internal viewer
image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
expect_null(viewImages(image, interactive=FALSE))

# Static visualisations
if (at_home() && requireNamespace("loder", quietly=TRUE))
{
    image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
    fov <- image$getFieldOfView()
    file <- ensureFileSuffix(tempfile(), "png")
    
    createSliceGraphic(image, z=30, device="png", file=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1])
    createProjectionGraphic(image, 3, device="png", file=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1])
    createContactSheetGraphic(image, 3, device="png", file=file, nColumns=8)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1]*8)
    tractor.base:::compositeImages(list(image), x=48, y=48, z=30, prefix=file)
    expect_equal(attr(loder::inspectPng(file),"width"), fov[1]*2)
}
