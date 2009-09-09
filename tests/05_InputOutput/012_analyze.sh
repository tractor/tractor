#@desc Checking that Analyze reader and file format convertor work
${TRACTOR} imageinfo data/analyze/nodif_brain | grep -v source
${TRACTOR} value data/analyze/nodif_brain 49 58 14
cp data/analyze/nodif_brain.* tmp/
${TRACTOR} chfiletype tmp/nodif_brain NIFTI_GZ
${TRACTOR} imageinfo tmp/nodif_brain | grep -v source
${TRACTOR} value tmp/nodif_brain 49 58 14
rm -f tmp/nodif_brain.*
