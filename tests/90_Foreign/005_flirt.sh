#@desc Checking FLIRT registration (FSL required)
${TRACTOR} reg-linear $TRACTOR_TEST_DATA/session/tractor/diffusion/refb0 $TRACTOR_HOME/share/tractor/mni/brain TransformName:transform Method:fsl EstimateOnly:true
${TRACTOR} reg-info transform | grep -v path
