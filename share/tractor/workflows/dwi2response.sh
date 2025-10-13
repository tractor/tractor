#! /bin/bash -e

#@command dwi2response
#@variables Algorithm, Order, Force

[ -n "$Algorithm" ] || Algorithm=tournier
[ -n "$Order" ] && TRACTOR_COMMAND_ARGS="$TRACTOR_COMMAND_ARGS -lmax $Order"
[ -n "$Force" ] || Force=0

force_arg=""
[ $Force -eq 0 ] || force_arg="-force"

source ${TRACTOR_HOME}/share/tractor/workflows/helpers/create-mrtrix-files.sh

set -x

case "$Algorithm" in
    tournier | tax) ${FURROW} ${TRACTOR_COMMAND} $Algorithm -fslgrad @mrtrix/bvecs @mrtrix/bvals -mask @mrtrix/mask ${TRACTOR_COMMAND_ARGS} @mrtrix/data @mrtrix/wmresp.txt $force_arg ;;
    dhollander) ${FURROW} ${TRACTOR_COMMAND} dhollander -fslgrad @mrtrix/bvecs @mrtrix/bvals -mask @mrtrix/mask ${TRACTOR_COMMAND_ARGS} @mrtrix/data @mrtrix/wmresp.txt @mrtrix/gmresp.txt @mrtrix/csfresp.txt $force_arg ;;
    *) echo "Error: The specified algorithm is not valid" 1>&2; exit 1 ;;
esac
