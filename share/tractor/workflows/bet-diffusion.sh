#! /bin/bash -e

#@command bet
#@variables IntensityThreshold, VerticalGradient

# Use defaults if environment variables aren't set
[ -z "$IntensityThreshold" ] && IntensityThreshold=0.3
[ -z "$VerticalGradient" ] && VerticalGradient=0

set -x
${FURROW} ${TRACTOR_COMMAND} @refb0 @maskedb0 -m -f "$IntensityThreshold" -g "$VerticalGradient"
result=`${FURROW} echo @maskedb0`
${TRACTOR} image mv "${result}_mask" @diffusion/mask
