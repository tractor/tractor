#@desc Checking graph decomposition
#@data data/graph/diffusion.Rdata
${TRACTOR} graph-reweight diffusion weightedFA
${TRACTOR} graph-decompose diffusion Method:modularity EdgeWeightThreshold:0.3
${TRACTOR} peek diffusion_partitioned
