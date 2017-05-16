#@desc Checking that we can apply a function to an image
${TRACTOR} apply $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA "sum(a>1,na.rm=TRUE)"
