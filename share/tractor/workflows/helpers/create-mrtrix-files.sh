diffusion_dir=`${FURROW} -z echo @diffusion/`
if [ -f "$diffusion_dir/directions.txt" ]; then
    directions="$diffusion_dir/directions.txt"
else
    data_stem=`${FURROW} -z -n echo @diffusion/data`
    if [ -f "${data_stem}.dirs" ]; then
        directions="${data_stem}.dirs"
    else
        echo "Error: Diffusion directions are not available" 1>&2
        exit 1
    fi
fi

set -x
${FURROW} mkdir -p @mrtrix/
${FURROW} -z test -f @mrtrix/data || ${TRACTOR} image link @diffusion/data @mrtrix/data
${FURROW} -z test -f @mrtrix/mask || ${TRACTOR} image link @diffusion/mask @mrtrix/mask
${FURROW} cp "$directions" @mrtrix/directions.txt
