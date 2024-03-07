reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

path <- system.file("extdata", "dicom", package="tractor.base")
temp <- tempdir()
file.copy(path, temp, recursive=TRUE)
path <- file.path(temp, "dicom")

sortDicomDirectories(path, deleteOriginals=TRUE)
expect_equal(length(list.files(path)), 2L)
expect_true(all(c("4630_DTIb3000s5","6157_fl3D_t1_sag") %in% list.files(path)))

files <- list.files(file.path(path,"6157_fl3D_t1_sag"), full.names=TRUE)
dicom <- readDicomFile(files[1])
expect_length(dicom$getAsciiFields("Version"), 1)
expect_stdout(print(dicom), "GROUP")

image <- readDicomDirectory(file.path(path,"6157_fl3D_t1_sag"))$image
expect_equal(image$getDimensions(), c(2,224,256))
expect_equal(image$getVoxelDimensions(), rep(1,3L))
expect_equal(round(image$getOrigin()), c(19,95,135))

reportr::setOutputLevel(Verbose)
expect_stdout(image <- readDicomDirectory(file.path(path,"6157_fl3D_t1_sag"), method="divest", interactive=FALSE)$image, "Found 2 DICOM")
expect_equal(image$getDimensions(), c(2,224,256))
expect_equal(image$getVoxelDimensions(), rep(1,3L))

unlink(path, recursive=TRUE)
