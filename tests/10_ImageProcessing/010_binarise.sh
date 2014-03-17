#@desc Checking that image binarising works
${TRACTOR} binarise data/session/tractor/diffusion/dti_FA tmp/dti_FA_binarised ThresholdLevel:0.5 ThresholdRelativeTo:nothing
${TRACTOR} value tmp/dti_FA_binarised 61 55 30
${TRACTOR} binarise data/session/tractor/diffusion/dti_FA tmp/dti_FA_binarised ThresholdLevel:0.5 ThresholdRelativeTo:max
${TRACTOR} value tmp/dti_FA_binarised 61 55 30
rm -f tmp/dti_FA_binarised.*
