#! /bin/bash -e

#@command dwi2response
#@variables Algorithm, Order

[ -n "$Algorithm" ] || Algorithm=tournier
[ -n "$Order" ] && TRACTOR_COMMAND_ARGS="$TRACTOR_COMMAND_ARGS -lmax $Order"

source ${TRACTOR_HOME}/share/tractor/workflows/helpers/create-mrtrix-files.sh

set -x

case "$Algorithm" in
    tournier | tax) ${FURROW} ${TRACTOR_COMMAND} $Algorithm -force -grad @mrtrix/directions.txt -mask @mrtrix/mask ${TRACTOR_COMMAND_ARGS} @mrtrix/data @mrtrix/wmresp.txt ;;
    dhollander) ${FURROW} ${TRACTOR_COMMAND} dhollander -force -grad @mrtrix/directions.txt -mask @mrtrix/mask ${TRACTOR_COMMAND_ARGS} @mrtrix/data @mrtrix/wmresp.txt @mrtrix/gmresp.txt @mrtrix/csfresp.txt ;;
    *) echo "Error: The specified algorithm is not valid" 1>&2; exit 1 ;;
esac
