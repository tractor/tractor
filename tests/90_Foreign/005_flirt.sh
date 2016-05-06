#@desc Checking FLIRT registration (FSL required)
${TRACTOR} reg-linear data/session/tractor/diffusion/refb0 ../share/mni/brain TransformName:tmp/transform Method:fsl EstimateOnly:true
${TRACTOR} reg-info tmp/transform | grep -v directory
rm -rf tmp/transform.xfmb
