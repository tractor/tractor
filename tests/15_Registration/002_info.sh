#@desc Checking that we can read transformations
${TRACTOR} reg-info data/session/tractor/transforms/diffusion2mni.xfmb | grep -v directory
