# Flatten upper triangular matrix
flattenUpperTri <- function(X) t(X)[upper.tri(X,diag=FALSE)]

# Get Strength
getStrength <- function (weight_matrix)
{
    strength <- rowSums(weight_matrix)
    return (strength)
}

randomNetworks <- function (graph)
{
    #Association Matrix
    weight_matrix <- graph$getAssociationMatrix()
    diag(weight_matrix) = 0
    
    ##### RUBINOV, SPORNS ALGORITHM STEP 1 #####
    # Binarize weight matrix to get vertices
    con_matrix <- (weight_matrix>0)*1
    n_vertices <- sum(con_matrix)
    nodes <- dim(weight_matrix)[1]
    pos_vertices <- nodes*nodes
    
    ##### RUBINOV, SPORNS ALGORITHM STEP 2 #####
    tri_weight_matrix <- flattenUpperTri(weight_matrix)
    rank_weights <- sort(tri_weight_matrix, decreasing = TRUE)
    
    # Initially all new weights are 0
    new_weights <- matrix(rep(0,pos_vertices), nodes, nodes)
    strength <- getStrength(weight_matrix)
    
    # Get Indices
    ij <- which((upper.tri(weight_matrix)*weight_matrix)>0, arr.ind=T)
    Lij <- nodes*(ij[,2]-1)+ij[,1]
    
    iterations <- sum(rank_weights>0)
    for (i in 1:(iterations-1))
    {
        # Calculate expected weights for each connection
        tri_new_weights <- upper.tri(new_weights, diag = FALSE) * new_weights
        
        # All possible combinations of nodes
        calc_weight <- strength - rowSums(tri_new_weights)
        exp_weight <- outer(calc_weight, calc_weight, FUN = "*")
        
        # Sort expected weights
        tmpRank <- seq(1:length(exp_weight[ij]))
        tmp <- cbind(exp_weight[ij], ij, tmpRank)
        order_exp_weights <- tmp[order(tmp[,1], decreasing = TRUE),]
        rank_exp_weights <- order_exp_weights[,4]
        
        # Get random vertex to update
        rand_con <- sample(seq(1,sum(rank_weights>0)), 1)
        nodeNum <- which((upper.tri(weight_matrix)*weight_matrix)==rank_weights[rand_con], arr.ind=T)
        
        # Find Vertex Value
        o <- rank_exp_weights[rand_con]
        
        # Save Location
        location <- c(Lij[o]%%nodes, Lij[o]%/%nodes+1)
        
        # Assign New weight
        new_weights[location[1], location[2]] <- rank_weights[rand_con]
        
        # Remove assigned weight from ranking vector
        rank_weights <- rank_weights[-rand_con]
        
        # Remove indices from further consideration
        ij <- ij[-o,]
        Lij <- Lij[-o]
    }
    new_weights <- projectTri(new_weights)
    return(new_weights)
}
