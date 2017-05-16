#@desc Checking affine decomposition consistency between TractoR and FSL
${TRACTOR} -v1 decompose $TRACTOR_TEST_DATA/session 2
echo "---"
sed -n '12,15p' $TRACTOR_TEST_DATA/session/tractor/fdt/data.ecclog >avscale.mat
${FSLDIR}/bin/avscale --allparams avscale.mat | head -n 18
