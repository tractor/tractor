#@desc Checking that NIfTI reader works
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/session/tractor/diffusion/maskedb0 | grep -v source
${TRACTOR} value $TRACTOR_TEST_DATA/session/tractor/diffusion/maskedb0 50 59 33
