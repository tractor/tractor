#@desc Checking that image binarising works
${TRACTOR} binarise $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA dti_FA_binarised ThresholdLevel:0.5 ThresholdRelativeTo:nothing
${TRACTOR} value dti_FA_binarised 61 55 30
${TRACTOR} binarise $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA dti_FA_binarised ThresholdLevel:0.5 ThresholdRelativeTo:max
${TRACTOR} value dti_FA_binarised 61 55 30
