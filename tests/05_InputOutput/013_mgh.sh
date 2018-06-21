#@desc Checking that MGH format reader and writer work
#@data data/session/tractor/diffusion/maskedb0.*
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/images/mgh/maskedb0 | grep -v source
${TRACTOR} value $TRACTOR_TEST_DATA/images/mgh/maskedb0 50 59 33
${TRACTOR} chfiletype maskedb0 MGH_GZ
${TRACTOR} imageinfo maskedb0 | grep -v source
${TRACTOR} value maskedb0 50 59 33
