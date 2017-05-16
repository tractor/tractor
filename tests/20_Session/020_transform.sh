#@desc Checking implicit registration
${TRACTOR} transform $TRACTOR_TEST_DATA/session 0 18 16 PointType:mm SourceSpace:MNI TargetSpace:diffusion
${TRACTOR} transform $TRACTOR_TEST_DATA/session 0 18 16 PointType:mm SourceSpace:MNI TargetSpace:diffusion Nearest:true
