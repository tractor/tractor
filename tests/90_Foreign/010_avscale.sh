#@desc Checking affine decomposition consistency between TractoR and FSL
${TRACTOR} -v1 decompose data/session 2
echo "---"
sed -n '12,15p' data/session/tractor/fdt/data.ecclog >tmp/avscale.mat
${FSLDIR}/bin/avscale --allparams tmp/avscale.mat | head -n 18
rm -f tmp/avscale.mat
