#@desc Checking that we can extract the values of an image within a region
${TRACTOR} mkroi $TRACTOR_TEST_DATA/session@FA 50 59 33 Width:3 ROIName:region
${TRACTOR} values $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA region
