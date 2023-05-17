.StandardBrainPath <- NULL

.Viewers <- c("tractor", "fsleyes", "fslview", "freeview", "mrview")

.AllSessionDirectories <- c("diffusion", "fdt", "bedpost", "transforms", "structural", "freesurfer", "functional")

.PrimarySessionDirectories <- c("diffusion", "structural", "functional")

.FdtDiffusionMap <- list(rawdata="basic", data="data", refb0="nodif", mask="nodif_brain_mask", maskedb0="nodif_brain", s0="dti_S0", fa="dti_FA", md="dti_MD", eigenvalue="dti_L%", eigenvector="dti_V%", axialdiff="dti_L1", radialdiff="dti_Lrad")

# The first element in each space is the default target
.RegistrationTargets <- list(mni=list(masked="brain"),
                             diffusion=list(masked="maskedb0",unmasked="refb0"),
                             structural=list(unmasked="refT1",masked="maskedT1"),
                             freesurfer=list(unmasked="normT1",masked="maskedT1"),
                             functional=list(unmasked="mean"))
