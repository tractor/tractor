#@desc Checking that "tractor -o" works
COLUMNS=200 ${TRACTOR} -o age | grep -v age.R
