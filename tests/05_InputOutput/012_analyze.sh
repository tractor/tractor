#@desc Checking that Analyze reader and file format convertor work
${TRACTOR} imageinfo data/analyze/maskedb0 | grep -v source
${TRACTOR} value data/analyze/maskedb0 49 58 14
cp data/analyze/maskedb0.* tmp/
${TRACTOR} chfiletype tmp/maskedb0 NIFTI_GZ
${TRACTOR} imageinfo tmp/maskedb0 | grep -v source
${TRACTOR} value tmp/maskedb0 49 58 14
rm -f tmp/maskedb0.*
