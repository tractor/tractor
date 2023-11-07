#! /bin/bash -e

#@command dwi2fod
#@prereq @mrtrix/wmresp.txt

algorithm=csd
mtargs=""

if ${FURROW} -z test -f @mrtrix/gmresp.txt; then
    algorithm=msmt_csd
    mtargs="$mtargs $(${FURROW} -z echo @mrtrix/gmresp.txt) $(${FURROW} -z echo @gmfod)"
fi

if ${FURROW} -z test -f @mrtrix/csfresp.txt; then
    algorithm=msmt_csd
    mtargs="$mtargs $(${FURROW} -z echo @mrtrix/csfresp.txt) $(${FURROW} -z echo @csffod)"
fi

set -x

${FURROW} ${TRACTOR_COMMAND} $algorithm -grad @mrtrix/directions.txt -mask @mrtrix/mask @mrtrix/data @mrtrix/wmresp.txt @wmfod.mif.gz "$mtargs"
