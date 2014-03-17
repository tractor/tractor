#@desc Checking that NIfTI reader works
${TRACTOR} imageinfo data/session/tractor/diffusion/maskedb0 | grep -v source
${TRACTOR} value data/session/tractor/diffusion/maskedb0 50 59 33
