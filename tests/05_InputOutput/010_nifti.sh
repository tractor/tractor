#@desc Checking that NIfTI reader works
${TRACTOR} imageinfo data/session-12dir/tractor/diffusion/maskedb0 | grep -v source
${TRACTOR} value data/session-12dir/tractor/diffusion/maskedb0 49 58 14
