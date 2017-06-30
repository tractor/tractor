#@desc Checking that we can create PNG images
${TRACTOR} slice $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA Z:33 GraphicName:fa_slice ColourScale:greyscale
${TRACTOR} value fa_slice.png 94 124 1
