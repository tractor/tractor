#@desc Checking that image smoothing works
cp data/session/tractor/diffusion/dti_FA.* tmp/
${TRACTOR} smooth tmp/dti_FA 5.0 WidthType:sd
${TRACTOR} value tmp/dti_FA_smoothed 61 55 30
${TRACTOR} smooth tmp/dti_FA 5.0 WidthType:fwhm
${TRACTOR} value tmp/dti_FA_smoothed 61 55 30
rm -f tmp/dti_FA*
