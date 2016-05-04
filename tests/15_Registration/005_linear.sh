#@desc Checking linear registration
${TRACTOR} reg-linear data/transform.xfmb/source data/transform.xfmb/target TransformationName:tmp/transform EstimateOnly:true | grep -v orientation | grep -v NiftyReg
${TRACTOR} reg-info tmp/transform | grep -v orientation
rm -rf tmp/transform.xfmb
