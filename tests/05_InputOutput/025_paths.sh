#@desc Checking path expansions
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/session@FA | grep -v source
${TRACTOR} imageinfo $TRACTOR_TEST_DATA/session@diffusion/data | grep -v source
