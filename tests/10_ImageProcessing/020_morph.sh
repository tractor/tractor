#@desc Checking mathematical morphology operations
${TRACTOR} binarise $TRACTOR_TEST_DATA/session@FA dti_FA ThresholdLevel:0.5 ThresholdRelativeTo:nothing
${TRACTOR} morph dti_FA 3 Operation:dilate KernelShape:box WidthUnit:vox
${TRACTOR} imagestats dti_FA_morphed Scope:nonzero
${TRACTOR} morph dti_FA 3 Operation:closing KernelShape:diamond WidthUnit:vox
${TRACTOR} imagestats dti_FA_morphed Scope:nonzero
