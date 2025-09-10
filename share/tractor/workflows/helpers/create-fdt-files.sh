set -x
${FURROW} mkdir -p @fdt/
${FURROW} -z test -f @fdt/data || ${TRACTOR} image map @diffusion/data @fdt/data
${FURROW} -z test -f @fdt/mask || ${TRACTOR} image map @diffusion/mask @fdt/mask
