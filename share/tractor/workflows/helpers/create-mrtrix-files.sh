set -x
${FURROW} mkdir -p @mrtrix/
${FURROW} -z test -f @mrtrix/data || ${TRACTOR} image map @mrtrix/data @diffusion/data
${FURROW} -z test -f @mrtrix/mask || ${TRACTOR} image map @mrtrix/mask @diffusion/mask
${TRACTOR} gradread @mrtrix/bvecs .
