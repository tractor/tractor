#@desc Checking that we can apply an existing transformation to an image
${TRACTOR} reg-apply $TRACTOR_TEST_DATA/session/tractor/diffusion/dti_FA dti_FA_mni TransformName:$TRACTOR_TEST_DATA/session/tractor/transforms/diffusion2mni
${TRACTOR} value dti_FA_mni 100 156 88
