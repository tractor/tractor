#@desc Checking that we can read a NIfTI-2 format file
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/images/nifti2/avg152T1_LR_nifti2 | grep -v source
${TRACTOR} value $TRACTOR_TEST_DATA/images/nifti2/avg152T1_LR_nifti2 53 64 46
