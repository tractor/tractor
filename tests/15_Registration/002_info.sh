#@desc Checking that we can read transformations
${TRACTOR} reg-info $TRACTOR_TEST_DATA/session/tractor/transforms/diffusion2mni.xfmb | grep -v directory
