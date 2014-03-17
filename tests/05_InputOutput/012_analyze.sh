#@desc Checking that Analyze reader and file format convertor work
${TRACTOR} imageinfo data/analyze/maskedb0 | grep -v source
${TRACTOR} value data/analyze/maskedb0 50 59 33
cp data/analyze/maskedb0.* tmp/
${TRACTOR} chfiletype tmp/maskedb0 NIFTI_GZ
${TRACTOR} imageinfo tmp/maskedb0 | grep -v source
${TRACTOR} value tmp/maskedb0 50 59 33
rm -f tmp/maskedb0.*
