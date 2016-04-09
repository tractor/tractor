#@desc Checking DICOM file sorting and reconstruction
${TRACTOR} -v1 dicomsort data/dicom DeleteOriginals:false && mv data/dicom/4630* data/dicom/6157* tmp/
${TRACTOR} -v1 -w tmp dicomread 6157_fl3D_t1_sag t1 | grep -v t1
${TRACTOR} value tmp/t1 1 145 139
rm -rf tmp/4630* tmp/6157* tmp/t1.*
