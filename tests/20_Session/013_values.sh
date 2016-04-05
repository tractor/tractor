#@desc Checking that we can extract the values of an image within a region
${TRACTOR} mkroi data/session@FA 50 59 33 Width:3 ROIName:tmp/region
${TRACTOR} values data/session/tractor/diffusion/dti_FA tmp/region
rm -f tmp/region.*
