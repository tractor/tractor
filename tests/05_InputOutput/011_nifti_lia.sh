#@desc Checking that NIfTI reader can handle nondiagonal xform matrices
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/images/nifti/maskedb0_lia | grep -v source
${TRACTOR} value $TRACTOR_TEST_DATA/images/nifti/maskedb0_lia 50 59 33
