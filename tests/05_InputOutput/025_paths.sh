#@desc Checking path expansions
${TRACTOR} imageinfo data/session@FA | grep -v source
${TRACTOR} imageinfo data/session@diffusion/data | grep -v source
