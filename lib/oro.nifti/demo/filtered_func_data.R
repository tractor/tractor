ffd <- readNIfTI(file.path(system.file("nifti", package="oro.nifti"), "ffd"))
image(ffd)
orthographic(ffd)
