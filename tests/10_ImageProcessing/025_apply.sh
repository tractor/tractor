#@desc Checking that we can apply general functions to images
${TRACTOR} apply $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA "sum(a>1,na.rm=TRUE)"
${TRACTOR} apply $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA "range(a,na.rm=TRUE)" | awk '{ print substr($1,1,6) }'

${TRACTOR} mkroi $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA 50 59 33 Width:3 ROIName:region
${TRACTOR} apply $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA region Combine:prod ResultName:FA_ROI
${TRACTOR} imagestats FA_ROI
