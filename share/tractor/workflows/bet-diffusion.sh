#! /bin/bash -e

#@command bet
#@variables IntensityThreshold, VerticalGradient

[ -n "$IntensityThreshold" ] || IntensityThreshold=0.3
[ -n "$VerticalGradient" ] || VerticalGradient=0

${FURROW} -z test -f @diffusion/refb0 || {
    echo "Error: A reference b=0 image file has not yet been created" 1>&2
    exit 1
}

result_stem=`${FURROW} -z -n -r echo @maskedb0`

set -x
${FURROW} -r ${TRACTOR_COMMAND} @refb0 @maskedb0 -m -f "$IntensityThreshold" -g "$VerticalGradient"
${TRACTOR} image mv "${result_stem}_mask" @diffusion/mask
