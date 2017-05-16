#@desc Checking that Analyze reader and file format convertor work
#@data data/analyze/maskedb0.*
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/analyze/maskedb0 | grep -v source
${TRACTOR} value $TRACTOR_TEST_DATA/analyze/maskedb0 50 59 33
${TRACTOR} chfiletype maskedb0 NIFTI_GZ
${TRACTOR} imageinfo maskedb0 | grep -v source
${TRACTOR} value maskedb0 50 59 33
