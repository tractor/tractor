betweennessCentrality <- function(weight_matrix){
  # Convert weight matrix in connection-length matrix
  # diag(weight_matrix) <- 0
  if(asGraph(weight_matrix)$isDirected()){
    stop("Betweenness Centrality is currently only implemented for undirected graphs")
  }
  nonzero_weights <- which(weight_matrix>0)
  weight_matrix[nonzero_weights] <- 1/(weight_matrix[nonzero_weights])
  nNodes <- dim(weight_matrix)[1]
  
  # Initialize betweenness centrality to 0 for all nodes
  betw_centr <- array(0, nNodes)
  
  # Initialization: for each node curNode
  for (curNode in 1:nNodes) {
    # 1. Mark curNode as unvisited by setting the distance between startNode and curNode to infiity
    dist_matrix <- array(Inf, nNodes)
    dist_matrix[curNode] <- 0
    
    # 2. Setlist of curNode's predecessors on a shortest Path to the empty list
    pred_matrix <- matrix(rep(F, (nNodes*nNodes)), nNodes, nNodes)
    
    # 3. Set the list of all shortest paths from startNode to curNode to the empty list 
    num_paths <- array(0, nNodes)
    num_paths[curNode] <- 1

    G1 <- weight_matrix
    S <- rep(T, nNodes)
    Queue <- rep(0, nNodes)
    q <- nNodes
    V <- curNode
    while (TRUE) {
      S[V] <- 0
      G1[,V] <- 0
      for (v in V){
        W <- which(as.logical(G1[v,]))
        Queue[q] <- v
        q <- q-1
        for (w in W) {
          DuW <- dist_matrix[v] + G1[v,w]
          if (DuW < dist_matrix[w]){
            dist_matrix[w] <- DuW
            num_paths[w] <- num_paths[v]
            pred_matrix[w,] <- 0
            pred_matrix[w,v] <- 1
          }
          else if (DuW == dist_matrix[w]){
            num_paths[w] <- num_paths[w] + num_paths[v]
            pred_matrix[w,v] <- 1
          }
        }
      }
      D <- dist_matrix[as.logical(S)]
      if (length(D) == 0) break 
      minD <- min(D)
      if (is.infinite(minD)){
        Queue[1:q] <- which(is.infinite(dist_matrix)) 
        break
      }
      V <- which(dist_matrix == minD) 
    }
    DP <- array(0,nNodes)
    for (w in Queue[1:(nNodes-1)]){
      betw_centr[w] <- betw_centr[w] + DP[w]
      for (v in which(pred_matrix[w,]>0)){
        DP[v] <- DP[v]+(1+DP[w]) * num_paths[v]/num_paths[w]
      }
    }
  }
  return(betw_centr)
}  

