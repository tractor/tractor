#@desc Checking the "divest" DICOM back-end
${TRACTOR} dicomsort $TRACTOR_TEST_DATA/images/dicom Method:divest Nested:false DeleteOriginals:false | grep -v dcm2niix
${TRACTOR} dicomread . Method:divest Interactive:false FileNames:metadata | grep -v dcm2niix
${FURROW} cat *.tags
