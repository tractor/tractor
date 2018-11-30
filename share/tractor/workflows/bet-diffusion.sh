#! /bin/bash -e

#@command bet
#@variables IntensityThreshold, VerticalGradient

[ -z "$IntensityThreshold" ] && IntensityThreshold=0.3
[ -z "$VerticalGradient" ] && VerticalGradient=0

${FURROW} -z test -f @diffusion/refb0 || {
    echo "Error: A reference b=0 image file has not yet been created" 1>&2
    exit 1
}

result=`${FURROW} -z echo @maskedb0`

set -x
${FURROW} ${TRACTOR_COMMAND} @refb0 @maskedb0 -m -f "$IntensityThreshold" -g "$VerticalGradient"
${TRACTOR} image mv "${result}_mask" @diffusion/mask
