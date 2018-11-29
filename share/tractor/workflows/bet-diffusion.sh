#! /bin/bash -e

#@command bet
#@variables IntensityThreshold, VerticalGradient

[ -z "$IntensityThreshold" ] && IntensityThreshold=0.3
[ -z "$VerticalGradient" ] && VerticalGradient=0

result=`${FURROW} -z echo @maskedb0`

set -x
${FURROW} ${TRACTOR_COMMAND} @refb0 @maskedb0 -m -f "$IntensityThreshold" -g "$VerticalGradient"
${TRACTOR} image mv "${result}_mask" @diffusion/mask
