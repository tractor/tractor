set -x
${FURROW} mkdir -p @mrtrix/
${FURROW} -z test -f @mrtrix/data || ${TRACTOR} image map @diffusion/data @mrtrix/data
${FURROW} -z test -f @mrtrix/mask || ${TRACTOR} image map @diffusion/mask @mrtrix/mask
${TRACTOR} gradread @mrtrix/bvecs .
