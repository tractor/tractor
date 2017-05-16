#@desc Checking region extraction
${TRACTOR} extract $TRACTOR_TEST_DATA/session/tractor/diffusion/maskedb0 225
${TRACTOR} which region eq 1
