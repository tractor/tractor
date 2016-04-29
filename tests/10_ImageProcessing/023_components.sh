#@desc Checking that we can find connected components
${TRACTOR} -v1 components data/nifti/boxes SortBySize:true
${TRACTOR} value boxes_components 40 40 28
${TRACTOR} -v1 components data/nifti/boxes SortBySize:true IncludeDiagonal:false
${TRACTOR} value boxes_components 40 40 28
rm -f boxes_components.*
