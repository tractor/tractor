#! /bin/bash -e

#@command eddy
#@prereq @fdt/acqparams.txt @fdt/index.txt

[ ${FURROW} -z test -f @diffusion/rawdata ] || {
    echo "Error: The specified session does not contain a raw data image" 1>&2
    exit 1
}

source helpers/create-fdt-files.sh

fdt_dir=`${FURROW} -z echo @fdt/`
topup_arg=""
[ ${FURROW} -z test -f @fdt/topup_fieldcoef ] && topup_arg="--topup=$fdt_dir/topup"

set -x
${FURROW} ${TRACTOR_COMMAND} --imain=@diffusion/rawdata --mask=@diffusion/mask --acqp=@fdt/acqparams.txt --index=@fdt/index.txt --bvecs=@fdt/bvecs --bvals=@fdt/bvals --out=@fdt/data "$topup_arg" >"$fdt_dir/eddy.log"
${TRACTOR} image mv @fdt/data @diffusion/data
${TRACTOR} image link @diffusion/daya @fdt/data
