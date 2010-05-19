#@desc Checking that DICOM reader works
${TRACTOR} dicomtags data/dicom/01.dcm | grep -v -E '(locale|Referenced Image Sequence|0x0088, 0x0200)'
