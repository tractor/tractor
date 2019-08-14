#@desc Checking affine decomposition consistency between TractoR and FSL
${TRACTOR} -v1 decompose $TRACTOR_TEST_DATA/session 2
echo "---"
${FURROW} sed -n '12,15p' $TRACTOR_TEST_DATA/session@fdt/data.ecclog >avscale.mat
${FURROW} avscale --allparams avscale.mat | head -n 18
