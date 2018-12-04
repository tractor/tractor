#! /bin/bash -e

#@command bedpostx
#@variables Context, FibresPerVoxel, ModelSpec

# Context is deprecated, but still allowed for now
[ -n "$Context" ] || Context="fg"
[ -n "$FibresPerVoxel" ] || FibresPerVoxel=3

source ${TRACTOR_HOME}/share/tractor/workflows/helpers/create-fdt-files.sh

bedpost_dir=`${FURROW} -z echo @bedpost/`

[ ! -d "$bedpost_dir" ] || rm -r "$bedpost_dir"

set -x

case "$Context" in
    fg) ${FURROW} ${TRACTOR_COMMAND} @fdt/ -n $FibresPerVoxel $ModelSpec ;;
    bg) ${FURROW} ${TRACTOR_COMMAND} @fdt/ -n $FibresPerVoxel $ModelSpec & ;;
    screen) screen -d -m ${FURROW} ${TRACTOR_COMMAND} @fdt/ -n $FibresPerVoxel $ModelSpec ;;
    *) echo "Error: The specified context is not valid" 1>&2; exit 1 ;;
esac
