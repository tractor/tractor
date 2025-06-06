#@desc Checking linear registration
${TRACTOR} reg-linear $TRACTOR_TEST_DATA/transform.xfmb/source $TRACTOR_TEST_DATA/transform.xfmb/target TransformName:transform EstimateOnly:true
${TRACTOR} reg-info transform | grep -v orient | grep -v path
