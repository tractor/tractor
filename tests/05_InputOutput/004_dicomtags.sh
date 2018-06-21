#@desc Checking that DICOM reader works
${TRACTOR} dicomtags $TRACTOR_TEST_DATA/images/dicom/01.dcm | grep -v -E '(locale|Referenced Image Sequence|0x0088, 0x0200)'
${TRACTOR} dicomtags $TRACTOR_TEST_DATA/images/dicom/01.dcm SiemensAscii:true
