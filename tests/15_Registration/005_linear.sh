#@desc Checking linear registration
${TRACTOR} reg-linear $TRACTOR_TEST_DATA/transform.xfmb/source $TRACTOR_TEST_DATA/transform.xfmb/target TransformName:transform EstimateOnly:true | grep -v orientation | grep -v NiftyReg
${TRACTOR} reg-info transform | grep -v orientation | grep -v directory
