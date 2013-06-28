#@desc Checking affine decomposition consistency between TractoR and FSL (FSL required)
${TRACTOR} -v1 decompose data/session-12dir 2
echo "---"
sed -n '12,15p' data/session-12dir/tractor/fdt/data.ecclog >tmp/avscale.mat
${FSLDIR}/bin/avscale --allparams tmp/avscale.mat | head -n 18
rm -f tmp/avscale.mat
