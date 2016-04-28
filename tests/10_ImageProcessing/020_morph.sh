#@desc Checking mathematical morphology operations
${TRACTOR} binarise data/session@FA tmp/dti_FA ThresholdLevel:0.5 ThresholdRelativeTo:nothing
${TRACTOR} morph tmp/dti_FA 3 Operation:dilate KernelShape:box WidthUnit:vox
${TRACTOR} imagestats dti_FA_morphed Scope:nonzero
${TRACTOR} morph tmp/dti_FA 3 Operation:closing KernelShape:diamond WidthUnit:vox
${TRACTOR} imagestats dti_FA_morphed Scope:nonzero
rm -f dti_FA_morphed.* tmp/dti_FA.*
