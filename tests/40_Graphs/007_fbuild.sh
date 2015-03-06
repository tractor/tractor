#@desc Checking that we can build graphs from functional data
${TRACTOR} graph-build data/session Type:functional GraphName:tmp/graph
${TRACTOR} peek tmp/graph
rm -f tmp/graph.Rdata
${TRACTOR} graph-build data/session Type:functional GraphName:tmp/graph UseShrinkage:true
${TRACTOR} peek tmp/graph
rm -f tmp/graph.Rdata
