#! /bin/bash -e

#@command dwi2response
#@variables Algorithm

[ -n "$Algorithm" ] || Algorithm=tournier

source ${TRACTOR_HOME}/share/tractor/workflows/helpers/create-mrtrix-files.sh

set -x

case "$Algorithm" in
    tournier | tax) ${FURROW} ${TRACTOR_COMMAND} $Algorithm -grad @mrtrix/directions.txt -mask @mrtrix/mask @mrtrix/data @mrtrix/wmresp.txt ;;
    dhollander) ${FURROW} ${TRACTOR_COMMAND} dhollander -grad @mrtrix/directions.txt -mask @mrtrix/mask @mrtrix/data @mrtrix/wmresp.txt @mrtrix/gmresp.txt @mrtrix/csfresp.txt ;;
    *) echo "Error: The specified algorithm is not valid" 1>&2; exit 1 ;;
esac
