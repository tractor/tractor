#! /bin/bash -e

#@command dwi2fod
#@prereq @mrtrix/wmresp.txt
#@variables Order

[ -n "$Order" ] && TRACTOR_COMMAND_ARGS="$TRACTOR_COMMAND_ARGS -lmax $Order"

algorithm=csd
mtargs=""

if ${FURROW} -z test -f @mrtrix/gmresp.txt; then
    algorithm=msmt_csd
    mtargs="$mtargs @mrtrix/gmresp.txt @gmfod.mif.gz"
fi

if ${FURROW} -z test -f @mrtrix/csfresp.txt; then
    algorithm=msmt_csd
    mtargs="$mtargs @mrtrix/csfresp.txt @csffod.mif.gz"
fi

set -x

${FURROW} ${TRACTOR_COMMAND} $algorithm -force -grad @mrtrix/directions.txt -mask @mrtrix/mask ${TRACTOR_COMMAND_ARGS} @mrtrix/data @mrtrix/wmresp.txt @wmfod.mif.gz $mtargs
