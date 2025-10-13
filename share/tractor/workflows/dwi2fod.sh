#! /bin/bash -e

#@command dwi2fod
#@prereq @mrtrix/wmresp.txt
#@variables Order, Force

[ -n "$Order" ] && TRACTOR_COMMAND_ARGS="$TRACTOR_COMMAND_ARGS -lmax $Order"
[ -n "$Force" ] || Force=0

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

force_arg=""
[ $Force -eq 0 ] || force_arg="-force"

set -x

${FURROW} ${TRACTOR_COMMAND} $algorithm -fslgrad @mrtrix/bvecs @mrtrix/bvals -mask @mrtrix/mask ${TRACTOR_COMMAND_ARGS} @mrtrix/data @mrtrix/wmresp.txt @wmfod.mif.gz $mtargs $force_arg
