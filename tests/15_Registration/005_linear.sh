#@desc Checking linear registration
${TRACTOR} reg-linear data/session/tractor/diffusion/refb0 ../share/mni/brain TransformationName:tmp/transform Levels:2 EstimateOnly:true
${TRACTOR} peek tmp/transform
rm -f tmp/transform*
