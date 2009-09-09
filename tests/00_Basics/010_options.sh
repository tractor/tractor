#@desc Checking that "tractor -o" works
${TRACTOR} -o list | grep -v list.R
