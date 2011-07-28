#@desc Checking that MGH format reader and writer work
${TRACTOR} imageinfo data/mgh/maskedb0 | grep -v source
${TRACTOR} value data/mgh/maskedb0 49 58 14
cp data/session-12dir/tractor/diffusion/maskedb0.* tmp/
${TRACTOR} chfiletype tmp/maskedb0 MGH_GZ
${TRACTOR} imageinfo tmp/maskedb0 | grep -v source
${TRACTOR} value tmp/maskedb0 49 58 14
rm -f tmp/maskedb0.*
