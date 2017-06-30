#@desc Checking that we can apply general functions to images
${TRACTOR} apply $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA "sum(a>1,na.rm=TRUE)"
${TRACTOR} apply $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA "range(a,na.rm=TRUE)" | awk '{ print substr($1,1,6) }'
