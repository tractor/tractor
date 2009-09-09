#@desc Checking that NIfTI reader works
${TRACTOR} imageinfo data/session-12dir/tractor/fdt/nodif_brain | grep -v source
${TRACTOR} value data/session-12dir/tractor/fdt/nodif_brain 49 58 14
