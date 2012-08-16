#@desc Checking that image smoothing works (mmand package required)
cp data/session-12dir/tractor/diffusion/dti_FA.* tmp/
${TRACTOR} smooth tmp/dti_FA 5.0 WidthType:sd
${TRACTOR} value tmp/dti_FA_smoothed 54 50 13
${TRACTOR} smooth tmp/dti_FA 5.0 WidthType:fwhm
${TRACTOR} value tmp/dti_FA_smoothed 54 50 13
rm -f tmp/dti_FA*
