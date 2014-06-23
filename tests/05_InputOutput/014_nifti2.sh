#@desc Checking that we can read a NIfTI-2 format file
${TRACTOR} imageinfo data/nifti2/avg152T1_LR_nifti2 | grep -v source
${TRACTOR} value data/nifti2/avg152T1_LR_nifti2 53 64 46
