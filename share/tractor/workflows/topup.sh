#! /bin/bash -e

#@command topup
#@prereq @fdt/acqparams.txt

${FURROW} -z test -f @diffusion/rawdata || {
    echo "Error: The specified session does not contain a raw data image" 1>&2
    exit 1
}

source helpers/create-fdt-files.sh

set -x
${FURROW} ${TRACTOR_COMMAND} --imain=@fdt/b0vols --datain=@fdt/acqparams.txt --config=b02b0.cnf --out=@fdt/topup --iout=@fdt/b0corrected
