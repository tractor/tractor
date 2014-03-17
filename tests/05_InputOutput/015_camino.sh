#@desc Checking that Camino file convertor works
cp data/session/tractor/diffusion/maskedb0.nii.gz tmp/
${TRACTOR} fsl2camino tmp/maskedb0.*
rm tmp/maskedb0.nii.gz
${TRACTOR} camino2fsl tmp/maskedb0.* data/session/tractor/diffusion/maskedb0.nii.gz 
mv tmp/maskedb0.*.nii.gz tmp/maskedb0.nii.gz
${TRACTOR} value tmp/maskedb0.nii.gz 50 59 33
rm -f tmp/maskedb0*
