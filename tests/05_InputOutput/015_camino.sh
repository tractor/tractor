#@desc Checking that Camino file convertor works
cp data/session-12dir/tractor/fdt/nodif_brain.nii.gz tmp/
${TRACTOR} fsl2camino tmp/nodif_brain.*
rm tmp/nodif_brain.nii.gz
${TRACTOR} camino2fsl tmp/nodif_brain.* data/session-12dir/tractor/fdt/nodif_brain.nii.gz 
mv tmp/nodif_brain.*.nii.gz tmp/nodif_brain.nii.gz
${TRACTOR} value tmp/nodif_brain.nii.gz 49 58 14
rm -f tmp/nodif_brain*
