#@desc Checking that image smoothing works
${TRACTOR} smooth data/session@FA 5.0 WidthType:sd
${TRACTOR} value dti_FA_smoothed 61 55 30
${TRACTOR} smooth data/session@FA 5.0 WidthType:fwhm
${TRACTOR} value dti_FA_smoothed 61 55 30
rm -f dti_FA_smoothed.*
