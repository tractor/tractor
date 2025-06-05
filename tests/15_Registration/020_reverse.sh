#@desc Checking that we can read transformations
${TRACTOR} reg-reverse $TRACTOR_TEST_DATA/session/tractor/transforms/diffusion2mni.xfmb
${TRACTOR} reg-info diffusion2mni_reversed | grep -v path
