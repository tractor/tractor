#@desc Checking that "tractor -o" works
COLUMNS=200 ${TRACTOR} -o list | grep -v list.R
