#! /bin/bash -e

#@command dtifit
#@variables WeightedLeastSquares

[ -n "$WeightedLeastSquares" ] || WeightedLeastSquares=0

source ${TRACTOR_HOME}/share/tractor/workflows/helpers/create-fdt-files.sh

wls_flag=""
[ $WeightedLeastSquares -eq 0 ] || wls_flag="-w"

set -x
${FURROW} ${TRACTOR_COMMAND} -k @fdt/data -m @fdt/mask -r @fdt/bvecs -b @fdt/bvals -o @fdt/dti --sse "$wls_flag"
${TRACTOR} image mv @fdt/s0 @diffusion/s0
${TRACTOR} image mv @fdt/fa @diffusion/fa
${TRACTOR} image mv @fdt/md @diffusion/md
${TRACTOR} image mv @fdt/sse @diffusion/sse
for i in 1 2 3; do
    ${TRACTOR} image mv @fdt/"eigenvalue%$i" @diffusion/"eigenvalue%$i"
    ${TRACTOR} image mv @fdt/"eigenvector%$i" @diffusion/"eigenvector%$i"
done
