#@desc Checking that MGH format reader and writer work
${TRACTOR} imageinfo data/mgh/maskedb0 | grep -v source
${TRACTOR} value data/mgh/maskedb0 50 59 33
cp data/session/tractor/diffusion/maskedb0.* tmp/
${TRACTOR} chfiletype tmp/maskedb0 MGH_GZ
${TRACTOR} imageinfo tmp/maskedb0 | grep -v source
${TRACTOR} value tmp/maskedb0 50 59 33
rm -f tmp/maskedb0.*
