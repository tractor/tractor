#! /bin/bash -e

#@command eddy_correct
#@variables ReferenceVolume

[ -n "$ReferenceVolume" ] || {
    echo "Error: Reference volume must be specified" 1>&2
    exit 1
}

${FURROW} -z test -f @diffusion/rawdata || {
    echo "Error: The specified session does not contain a raw data image" 1>&2
    exit 1
}

set -x
${FURROW} ${TRACTOR_COMMAND} @diffusion/rawdata @diffusion/data $ReferenceVolume
${FURROW} mkdir -p @fdt/
${FURROW} mv @diffusion/data.ecclog @fdt/data.ecclog
