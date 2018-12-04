set -x
${FURROW} mkdir -p @fdt/
${FURROW} -z test -f @fdt/data || ${TRACTOR} image link @diffusion/data @fdt/data
${FURROW} -z test -f @fdt/mask || ${TRACTOR} image link @diffusion/mask @fdt/mask
