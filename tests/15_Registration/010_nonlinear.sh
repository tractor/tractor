#@desc Checking nonlinear registration
#@data data/transform.xfmb
${TRACTOR} reg-nonlinear $TRACTOR_TEST_DATA/transform.xfmb/source $TRACTOR_TEST_DATA/transform.xfmb/target TransformName:transform EstimateOnly:true | grep -v orient
${TRACTOR} reg-info transform | grep -v orient | grep -v path
