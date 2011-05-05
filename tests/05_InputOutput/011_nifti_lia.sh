#@desc Checking that NIfTI reader can handle nondiagonal xform matrices
${TRACTOR} imageinfo data/nifti/nodif_brain_lia | grep -v source
${TRACTOR} value data/nifti/nodif_brain_lia 49 58 14
