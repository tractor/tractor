#@desc Checking that image binarising works
${TRACTOR} binarise data/session-12dir/tractor/diffusion/dti_FA tmp/dti_FA_binarised ThresholdLevel:0.5 ThresholdRelativeTo:nothing
${TRACTOR} value tmp/dti_FA_binarised 54 50 13
${TRACTOR} binarise data/session-12dir/tractor/diffusion/dti_FA tmp/dti_FA_binarised ThresholdLevel:0.5 ThresholdRelativeTo:max
${TRACTOR} value tmp/dti_FA_binarised 54 50 13
rm -f tmp/dti_FA_binarised.*
