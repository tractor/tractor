#@desc Checking that parallelised code works
${TRACTOR} -p 2 hello-parallel | grep -v WARNING
