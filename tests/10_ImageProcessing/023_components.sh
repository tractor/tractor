#@desc Checking that we can find connected components
cp data/nifti/boxes.* tmp/
${TRACTOR} -v1 components tmp/boxes SortBySize:true
${TRACTOR} value tmp/boxes_components 40 40 28
${TRACTOR} -v1 components tmp/boxes SortBySize:true IncludeDiagonal:false
${TRACTOR} value tmp/boxes_components 40 40 28
rm -f tmp/boxes*
