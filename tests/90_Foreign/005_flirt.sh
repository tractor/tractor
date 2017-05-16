#@desc Checking FLIRT registration (FSL required)
${TRACTOR} reg-linear $TRACTOR_TEST_DATA/session/tractor/diffusion/refb0 $TRACTOR_TEST_DATA/../share/tractor/mni/brain TransformName:transform Method:fsl EstimateOnly:true
${TRACTOR} reg-info transform | grep -v directory
