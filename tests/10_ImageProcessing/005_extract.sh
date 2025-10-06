#@desc Checking region extraction
${TRACTOR} extract $TRACTOR_TEST_DATA/session/tractor/diffusion/parcellation 251
${TRACTOR} which region eq 1
