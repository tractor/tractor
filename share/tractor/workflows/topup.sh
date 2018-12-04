#! /bin/bash -e

#@command topup
#@prereq @fdt/acqparams.txt

set -x
${FURROW} ${TRACTOR_COMMAND} --imain=@fdt/b0vols --datain=@fdt/acqparams.txt --config=b02b0.cnf --out=@fdt/topup --iout=@fdt/b0corrected
