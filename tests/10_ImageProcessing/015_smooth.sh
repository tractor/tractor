#@desc Checking that image smoothing works
${TRACTOR} smooth $TRACTOR_TEST_DATA/session@FA 5.0 WidthType:sd
${TRACTOR} value dti_FA_smoothed 61 55 30
${TRACTOR} smooth $TRACTOR_TEST_DATA/session@FA 5.0 WidthType:fwhm
${TRACTOR} value dti_FA_smoothed 61 55 30
