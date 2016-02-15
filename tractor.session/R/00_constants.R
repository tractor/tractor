.StandardBrainPath <- NULL

.Viewers <- c("tractor", "fslview", "freeview")

.AllSessionDirectories <- c("diffusion", "fdt", "bedpost", "transforms", "structural", "freesurfer", "functional")

.PrimarySessionDirectories <- c("diffusion", "structural", "functional")

.FdtDiffusionMap <- list(rawdata="basic", data="data", refb0="nodif", mask="nodif_brain_mask", maskedb0="nodif_brain", s0="dti_S0", fa="dti_FA", md="dti_MD", eigenvalue="dti_L%", eigenvector="dti_V%", axialdiff="dti_L1", radialdiff="dti_Lrad")

.RegistrationTargets <- list(mni="brain", diffusion="maskedb0", structural="reft1", freesurfer="normT1", functional="mean")
