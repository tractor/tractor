#@desc Checking that we can build a graph from streamlines
${TRACTOR} graph-build data/session TractName:data/streamlines/wm2gm GraphName:tmp/graph
${TRACTOR} peek tmp/graph
rm -f tmp/graph.Rdata
