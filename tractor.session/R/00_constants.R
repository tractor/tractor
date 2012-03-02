.StandardBrainPath <- NULL

.Camino <- list(
    typeNames=c("byte",     "char",     "short",    "int",      "long",     "float",    "double"),
    rTypes=c(   "integer",  "integer",  "integer",  "integer",  "integer",  "double",   "double"),
    sizes=c(     1,          1,          2,          4,          8,          4,          8),
    isSigned=c(  TRUE,       FALSE,      TRUE,       TRUE,       TRUE,       TRUE,       TRUE)
)

.DefaultSessionDirectories <- list(diffusion="diffusion", camino="camino", fdt="fdt", bedpost="fdt.bedpostX", probtrack="fdt.track")

# All file names which do not contain a '.' are assumed to indicate images by
# standardiseSessionHierarchy(), and possibly other functions
.DefaultSessionMap <- list(
    diffusion=list(rawdata="rawdata", data="data", refb0="refb0", mask="mask", maskedb0="maskedb0", s0="dti_S0", fa="dti_FA", md="dti_MD", eigenvalue="dti_eigval%", eigenvector="dti_eigvec%", axialdiff="dti_eigval1", radialdiff="dti_radial", sse="dti_SSE"),
    fdt=list(data="data", mask="nodif_brain_mask", s0="dti_S0", fa="dti_FA", md="dti_MD", eigenvalue="dti_L%", eigenvector="dti_V%", sse="dti_sse"),
    bedpost=list(dyads="dyads%", avf="mean_f%samples", theta="mean_th%samples", phi="mean_ph%samples", avfsamples="merged_f%samples", thetasamples="merged_th%samples", phisamples="merged_ph%samples")
)

.FdtDiffusionMap <- list(rawdata="basic", data="data", refb0="nodif", mask="nodif_brain_mask", maskedb0="nodif_brain", s0="dti_S0", fa="dti_FA", md="dti_MD", eigenvalue="dti_L%", eigenvector="dti_V%", axialdiff="dti_L1", radialdiff="dti_Lrad")
