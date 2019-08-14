#@desc Checking that we can copy an image and its auxiliary files
${TRACTOR} image copy $TRACTOR_TEST_DATA/session@diffusion/parcellation .
${FURROW} ls -1 | sort
