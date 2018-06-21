#@desc Checking DICOM file sorting and reconstruction
#@data data/images/dicom
${TRACTOR} -v1 dicomsort dicom
${TRACTOR} -v1 dicomread dicom/6157_fl3D_t1_sag t1 | grep -v t1
${TRACTOR} value t1 1 145 139
