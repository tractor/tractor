#@desc Checking the "divest" DICOM back-end
${TRACTOR} dicomsort $TRACTOR_TEST_DATA/images/dicom Method:divest Nested:false DeleteOriginals:false
${TRACTOR} dicomread . Method:divest Interactive:false FileNames:metadata | grep -v dcm2niix
${TRACTOR} passthrough cat *.tags
