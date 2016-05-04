#@desc Checking that we can apply an existing transformation to an image
${TRACTOR} reg-apply data/session/tractor/diffusion/dti_FA tmp/dti_FA_mni TransformationName:data/session/tractor/transforms/diffusion2mni
${TRACTOR} value tmp/dti_FA_mni 100 156 88
rm -f tmp/dti_FA_mni*
