#! /bin/bash -e

#@command bedpostx
#@variables FibresPerVoxel, ModelSpec

[ -n "$FibresPerVoxel" ] || FibresPerVoxel=3

bedpost_dir=`${FURROW} -z echo @bedpost/`

[ ! -d "$bedpost_dir" ] || rm -r "$bedpost_dir"

source ${TRACTOR_HOME}/share/tractor/workflows/helpers/create-fdt-files.sh

set -x

${FURROW} -r ${TRACTOR_COMMAND} @fdt/ -n $FibresPerVoxel $ModelSpec
