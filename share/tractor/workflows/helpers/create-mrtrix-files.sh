set -x
${FURROW} mkdir -p @mrtrix/
${FURROW} -z test -f @mrtrix/data || ${TRACTOR} image link @diffusion/data @mrtrix/data
${FURROW} -z test -f @mrtrix/mask || ${TRACTOR} image link @diffusion/mask @mrtrix/mask
${TRACTOR} gradread @mrtrix/bvecs .
