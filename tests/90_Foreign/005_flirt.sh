#@desc Checking FLIRT registration (FSL required)
${TRACTOR} reg-linear data/session-12dir/tractor/diffusion/refb0 ../share/mni/brain TransformationName:tmp/transform Method:fsl EstimateOnly:true
${TRACTOR} peek tmp/transform
echo "---"
${TRACTOR} affinedump tmp/transform
rm -f tmp/transform*
