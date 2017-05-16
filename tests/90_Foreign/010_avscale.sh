#@desc Checking affine decomposition consistency between TractoR and FSL
${TRACTOR} -v1 decompose $TRACTOR_TEST_DATA/session 2
echo "---"
# This is slightly fragile, because we have to hard-code the path for FSL
cd tmp/90_Foreign/010_avscale
sed -n '12,15p' $TRACTOR_TEST_DATA/session/tractor/fdt/data.ecclog >avscale.mat
${FSLDIR}/bin/avscale --allparams avscale.mat | head -n 18
