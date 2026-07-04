#@desc Checking uninitialised nonlinear registration
${TRACTOR} reg-nonlinear $TRACTOR_TEST_DATA/transform.xfmb/source $TRACTOR_TEST_DATA/transform.xfmb/target TransformName:transform EstimateOnly:true | grep -v NiftyReg
${TRACTOR} reg-info transform | grep -v orient | grep -v path
