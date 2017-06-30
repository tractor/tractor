#@desc Checking the "divest" DICOM back-end
${TRACTOR} dicomread $TRACTOR_TEST_DATA/dicom Method:divest Interactive:false | grep -v dcm2niix
${TRACTOR} passthrough cat *.tags
