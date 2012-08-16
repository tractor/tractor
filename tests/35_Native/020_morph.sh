#@desc Checking mathematical morphology operations
cp data/session-12dir/tractor/diffusion/dti_FA.* tmp/
${TRACTOR} morph tmp/dti_FA 3 Operation:dilate KernelShape:box WidthUnit:vox
${TRACTOR} imagestats tmp/dti_FA_morphed Scope:nonzero
${TRACTOR} morph tmp/dti_FA 3 Operation:closing KernelShape:diamond WidthUnit:vox
${TRACTOR} imagestats tmp/dti_FA_morphed Scope:nonzero
rm -f tmp/dti_FA*
