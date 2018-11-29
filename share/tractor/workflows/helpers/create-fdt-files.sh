set -x
${FURROW} mkdir -p @fdt/
[ ${FURROW} -z test -f @diffusion/data -a ! -f @fdt/data ] && ${TRACTOR} image link @diffusion/data @fdt/data
[ ${FURROW} -z test -f @diffusion/mask -a ! -f @fdt/mask ] && ${TRACTOR} image link @diffusion/mask @fdt/mask
set +x
