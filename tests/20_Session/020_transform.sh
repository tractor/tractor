#@desc Checking implicit registration
${TRACTOR} transform data/session 0 18 16 PointType:mm SourceSpace:MNI TargetSpace:diffusion
${TRACTOR} transform data/session 0 18 16 PointType:mm SourceSpace:MNI TargetSpace:diffusion Nearest:true
