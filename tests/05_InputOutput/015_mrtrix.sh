#@desc Checking that we can read an MRtrix image file
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/images/mrtrix/maskedb0 | grep -v source
${TRACTOR} value $TRACTOR_TEST_DATA/images/mrtrix/maskedb0 50 59 33
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/images/mrtrix/maskedb0_lia | grep -v source
${TRACTOR} value $TRACTOR_TEST_DATA/images/mrtrix/maskedb0_lia 50 59 33
