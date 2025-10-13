#! /bin/bash -e

#@command recon-all
#@variables FreesurferOptions

[ -n "$SUBJECTS_DIR" ] || {
    echo "Error: Freesurfer does not seem to be properly configured: can't identify subjects directory" 1>&2
    exit 1
}

session_name=`basename "$TRACTOR_SESSION_PATH"`
[ ! -e "$SUBJECTS_DIR/$session_name" ] || {
    echo "FreeSurfer subject named $session_name already exists" 1>&2
    exit 1
}

target_dir=`${FURROW} -rz echo @freesurfer/mri/orig`

set -x
${FURROW} mkdir -p "$target_dir"
for i in `seq 1 99`; do
    target=`printf "${target_dir}/%03d" $i`
    ${FURROW} -z test -f "@t1%$i" || break
    ${TRACTOR} image cp "@t1%$i" "$target"
    ${TRACTOR} chfiletype "$target" MGH_GZ
done
${FURROW} ln -s @freesurfer/ "$SUBJECTS_DIR/$session_name"
${TRACTOR_COMMAND} -subjid "$session_name" "$FreesurferOptions"
rm -f "$SUBJECTS_DIR/$session_name"
