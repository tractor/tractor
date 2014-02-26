graph.empty <- function(n=0, directed=TRUE) {
  # Argument checks
  n <- as.integer(n)
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_empty", n, directed,
        PACKAGE="igraph")

  res
}

vcount <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_vcount", graph,
        PACKAGE="igraph")


  res
}

graph.full.citation <- function(n, directed=TRUE) {
  # Argument checks
  n <- as.integer(n)
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_full_citation", n, directed,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'Full citation graph')
  res
}

graph.lcf <- function(n, shifts, repeats=1) {
  # Argument checks
  n <- as.integer(n)
  shifts <- as.numeric(shifts)
  repeats <- as.integer(repeats)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_lcf_vector", n, shifts, repeats,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'LCF graph')
  res
}

graph.adjlist <- function(adjlist, mode=c("out", "in", "all", "total"), duplicate=TRUE) {
  # Argument checks
  adjlist <- lapply(adjlist, function(x) as.integer(x)-1L)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  duplicate <- as.logical(duplicate)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_adjlist", adjlist, mode, duplicate,
        PACKAGE="igraph")

  res
}

forest.fire.game <- function(nodes, fw.prob, bw.factor=1, ambs=1, directed=TRUE) {
  # Argument checks
  nodes <- as.integer(nodes)
  fw.prob <- as.numeric(fw.prob)
  bw.factor <- as.numeric(bw.factor)
  ambs <- as.integer(ambs)
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_forest_fire_game", nodes, fw.prob, bw.factor, ambs, directed,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'Forest fire model')
  res <- set.graph.attribute(res, 'fw.prob', fw.prob)
  res <- set.graph.attribute(res, 'bw.factor', bw.factor)
  res <- set.graph.attribute(res, 'ambs', ambs)
  res
}

interconnected.islands.game <- function(islands.n, islands.size, islands.pin, n.inter) {
  # Argument checks
  islands.n <- as.integer(islands.n)
  islands.size <- as.integer(islands.size)
  islands.pin <- as.numeric(islands.pin)
  n.inter <- as.integer(n.inter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_simple_interconnected_islands_game", islands.n, islands.size, islands.pin, n.inter,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'Interconnected islands model')
  res <- set.graph.attribute(res, 'islands.n', islands.n)
  res <- set.graph.attribute(res, 'islands.size', islands.size)
  res <- set.graph.attribute(res, 'islands.pin', islands.pin)
  res <- set.graph.attribute(res, 'n.inter', n.inter)
  res
}

static.fitness.game <- function(no.of.edges, fitness.out, fitness.in=NULL, loops=FALSE, multiple=FALSE) {
  # Argument checks
  no.of.edges <- as.integer(no.of.edges)
  fitness.out <- as.numeric(fitness.out)
  if (!is.null(fitness.in)) fitness.in <- as.numeric(fitness.in)
  loops <- as.logical(loops)
  multiple <- as.logical(multiple)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_static_fitness_game", no.of.edges, fitness.out, fitness.in, loops, multiple,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'Static fitness model')
  res <- set.graph.attribute(res, 'loops', loops)
  res <- set.graph.attribute(res, 'multiple', multiple)
  res
}

static.power.law.game <- function(no.of.nodes, no.of.edges, exponent.out, exponent.in=-1, loops=FALSE, multiple=FALSE, finite.size.correction=TRUE) {
  # Argument checks
  no.of.nodes <- as.integer(no.of.nodes)
  no.of.edges <- as.integer(no.of.edges)
  exponent.out <- as.numeric(exponent.out)
  exponent.in <- as.numeric(exponent.in)
  loops <- as.logical(loops)
  multiple <- as.logical(multiple)
  finite.size.correction <- as.logical(finite.size.correction)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_static_power_law_game", no.of.nodes, no.of.edges, exponent.out, exponent.in, loops, multiple, finite.size.correction,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'Static power law model')
  res <- set.graph.attribute(res, 'exponent.out', exponent.out)
  res <- set.graph.attribute(res, 'exponent.in', exponent.in)
  res <- set.graph.attribute(res, 'loops', loops)
  res <- set.graph.attribute(res, 'multiple', multiple)
  res <- set.graph.attribute(res, 'finite.size.correction', finite.size.correction)
  res
}

k.regular.game <- function(no.of.nodes, k, directed=FALSE, multiple=FALSE) {
  # Argument checks
  no.of.nodes <- as.integer(no.of.nodes)
  k <- as.integer(k)
  directed <- as.logical(directed)
  multiple <- as.logical(multiple)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_k_regular_game", no.of.nodes, k, directed, multiple,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'k-regular graph')
  res <- set.graph.attribute(res, 'k', k)
  res
}

sbm.game <- function(n, pref.matrix, block.sizes, directed=FALSE, loops=FALSE) {
  # Argument checks
  n <- as.integer(n)
  pref.matrix <- as.matrix(structure(as.double(pref.matrix), dim=dim(pref.matrix)))
  block.sizes <- as.integer(block.sizes)
  directed <- as.logical(directed)
  loops <- as.logical(loops)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_sbm_game", n, pref.matrix, block.sizes, directed, loops,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'Stochastic block-model')
  res <- set.graph.attribute(res, 'loops', loops)
  res
}

closeness.estimate <- function(graph, vids=V(graph), mode=c("out", "in", "all", "total"), cutoff, weights=NULL, normalized=FALSE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  cutoff <- as.numeric(cutoff)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  normalized <- as.logical(normalized)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_closeness_estimate", graph, vids-1, mode, cutoff, weights, normalized,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

betweenness.estimate <- function(graph, vids=V(graph), directed=TRUE, cutoff, weights=NULL, nobigint=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  directed <- as.logical(directed)
  cutoff <- as.numeric(cutoff)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  nobigint <- as.logical(nobigint)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_betweenness_estimate", graph, vids-1, directed, cutoff, weights, nobigint,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

page.rank.old <- function(graph, vids=V(graph), directed=TRUE, niter=1000, eps=0.001, damping=0.85, old=FALSE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  directed <- as.logical(directed)
  niter <- as.integer(niter)
  eps <- as.numeric(eps)
  damping <- as.numeric(damping)
  old <- as.logical(old)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_pagerank_old", graph, vids-1, directed, niter, eps, damping, old,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

page.rank <- function(graph, algo=c("prpack", "arpack", "power"), vids=V(graph), directed=TRUE, damping=0.85, personalized=NULL, weights=NULL, options=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  algo <- switch(igraph.match.arg(algo), "power"=0L, "arpack"=1L, 
  "prpack"=2L)
  vids <- as.igraph.vs(graph, vids)
  directed <- as.logical(directed)
  damping <- as.numeric(damping)
  if (!is.null(personalized)) personalized <- as.numeric(personalized)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  if (is.null(options)) {                         
  if (algo == 0L) {                         
  options <- list(niter=1000, eps=0.001)      
  } else if (algo == 1L) {                  
  options <- igraph.arpack.default            
  } else {                                  
  options <- NULL                             
  }                                         
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_personalized_pagerank", graph, algo, vids-1, directed, damping, personalized, weights, options,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res$vector) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

induced.subgraph <- function(graph, vids, impl=c("auto", "copy_and_delete", "create_from_scratch")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  impl <- switch(igraph.match.arg(impl), "auto"=0, "copy_and_delete"=1, "create_from_scratch"=2)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_induced_subgraph", graph, vids-1, impl,
        PACKAGE="igraph")

  res
}

subgraph.edges <- function(graph, eids, delete.vertices=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  eids <- as.igraph.es(graph, eids)
  delete.vertices <- as.logical(delete.vertices)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_subgraph_edges", graph, eids-1, delete.vertices,
        PACKAGE="igraph")

  res
}

path.length.hist <- function(graph, directed=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_path_length_hist", graph, directed,
        PACKAGE="igraph")

  res
}

simplify <- function(graph, remove.multiple=TRUE, remove.loops=TRUE, edge.attr.comb=getIgraphOpt("edge.attr.comb")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  remove.multiple <- as.logical(remove.multiple)
  remove.loops <- as.logical(remove.loops)
  edge.attr.comb <- igraph.i.attribute.combination(edge.attr.comb)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_simplify", graph, remove.multiple, remove.loops, edge.attr.comb,
        PACKAGE="igraph")

  res
}

is.dag <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_dag", graph,
        PACKAGE="igraph")

  res
}

is.simple <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_simple", graph,
        PACKAGE="igraph")

  res
}

has.multiple <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_has_multiple", graph,
        PACKAGE="igraph")

  res
}

evcent <- function(graph, directed=FALSE, scale=TRUE, weights=NULL, options=igraph.arpack.default) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)
  scale <- as.logical(scale)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  options.tmp <- igraph.arpack.default; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_eigenvector_centrality", graph, directed, scale, weights, options,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res$vector) <- get.vertex.attribute(graph, "name", ) 
  }
  res
}

hub.score <- function(graph, scale=TRUE, weights=NULL, options=igraph.arpack.default) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  scale <- as.logical(scale)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  options.tmp <- igraph.arpack.default; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_hub_score", graph, scale, weights, options,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res$vector) <- get.vertex.attribute(graph, "name", ) 
  }
  res
}

authority.score <- function(graph, scale=TRUE, weights=NULL, options=igraph.arpack.default) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  scale <- as.logical(scale)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  options.tmp <- igraph.arpack.default; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_authority_score", graph, scale, weights, options,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res$vector) <- get.vertex.attribute(graph, "name", ) 
  }
  res
}

arpack.unpack.complex <- function(vectors, values, nev) {
  # Argument checks
  vectors <- as.matrix(structure(as.double(vectors), dim=dim(vectors)))
  values <- as.matrix(structure(as.double(values), dim=dim(values)))
  nev <- as.integer(nev)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_arpack_unpack_complex", vectors, values, nev,
        PACKAGE="igraph")

  res
}

is.mutual <- function(graph, es=E(graph)) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  es <- as.igraph.es(graph, es)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_mutual", graph, es-1,
        PACKAGE="igraph")

  res
}

maximum.cardinality.search <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_maximum_cardinality_search", graph,
        PACKAGE="igraph")

  res
}

graph.knn <- function(graph, vids=V(graph), weights=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_avg_nearest_neighbor_degree", graph, vids-1, weights,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res$knn) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

graph.strength <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total"), loops=TRUE, weights=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_strength", graph, vids-1, mode, loops, weights,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

centralize.scores <- function(scores, theoretical.max=0, normalized=TRUE) {
  # Argument checks
  scores <- as.numeric(scores)
  theoretical.max <- as.numeric(theoretical.max)
  normalized <- as.logical(normalized)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization", scores, theoretical.max, normalized,
        PACKAGE="igraph")


  res
}

centralization.degree <- function(graph, mode=c("all", "out", "in", "total"), loops=TRUE, normalized=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)
  normalized <- as.logical(normalized)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_degree", graph, mode, loops, normalized,
        PACKAGE="igraph")

  res
}

centralization.degree.tmax <- function(graph=NULL, nodes=0, mode=c("all", "out", "in", "total"), loops=FALSE) {
  # Argument checks
  if (!is.null(graph) && !is.igraph(graph)) { stop("Not a graph object") }
  nodes <- as.integer(nodes)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_degree_tmax", graph, nodes, mode, loops,
        PACKAGE="igraph")

  res
}

centralization.betweenness <- function(graph, directed=TRUE, nobigint=TRUE, normalized=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)
  nobigint <- as.logical(nobigint)
  normalized <- as.logical(normalized)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_betweenness", graph, directed, nobigint, normalized,
        PACKAGE="igraph")

  res
}

centralization.betweenness.tmax <- function(graph=NULL, nodes=0, directed=TRUE) {
  # Argument checks
  if (!is.null(graph) && !is.igraph(graph)) { stop("Not a graph object") }
  nodes <- as.integer(nodes)
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_betweenness_tmax", graph, nodes, directed,
        PACKAGE="igraph")

  res
}

centralization.closeness <- function(graph, mode=c("out", "in", "all", "total"), normalized=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  normalized <- as.logical(normalized)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_closeness", graph, mode, normalized,
        PACKAGE="igraph")

  res
}

centralization.closeness.tmax <- function(graph=NULL, nodes=0, mode=c("out", "in", "all", "total")) {
  # Argument checks
  if (!is.null(graph) && !is.igraph(graph)) { stop("Not a graph object") }
  nodes <- as.integer(nodes)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_closeness_tmax", graph, nodes, mode,
        PACKAGE="igraph")

  res
}

centralization.evcent <- function(graph, directed=FALSE, scale=TRUE, options=igraph.arpack.default, normalized=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)
  scale <- as.logical(scale)
  options.tmp <- igraph.arpack.default; options.tmp[ names(options) ] <- options ; options <- options.tmp
  normalized <- as.logical(normalized)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_eigenvector_centrality", graph, directed, scale, options, normalized,
        PACKAGE="igraph")

  res
}

centralization.evcent.tmax <- function(graph=NULL, nodes=0, directed=FALSE, scale=TRUE) {
  # Argument checks
  if (!is.null(graph) && !is.igraph(graph)) { stop("Not a graph object") }
  nodes <- as.integer(nodes)
  directed <- as.logical(directed)
  scale <- as.logical(scale)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_centralization_eigenvector_centrality_tmax", graph, nodes, directed, scale,
        PACKAGE="igraph")

  res
}

assortativity.nominal <- function(graph, types, directed=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  types <- as.numeric(types)-1
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_assortativity_nominal", graph, types, directed,
        PACKAGE="igraph")

  res
}

assortativity <- function(graph, types1, types2=NULL, directed=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  types1 <- as.numeric(types1)
  if (!is.null(types2)) types2 <- as.numeric(types2)
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_assortativity", graph, types1, types2, directed,
        PACKAGE="igraph")

  res
}

assortativity.degree <- function(graph, directed=TRUE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_assortativity_degree", graph, directed,
        PACKAGE="igraph")

  res
}

contract.vertices <- function(graph, mapping, vertex.attr.comb=getIgraphOpt("vertex.attr.comb")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  mapping <- as.numeric(mapping)-1
  vertex.attr.comb <- igraph.i.attribute.combination(vertex.attr.comb)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_contract_vertices", graph, mapping, vertex.attr.comb,
        PACKAGE="igraph")

  res
}

eccentricity <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_eccentricity", graph, vids-1, mode,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

radius <- function(graph, mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_radius", graph, mode,
        PACKAGE="igraph")

  res
}

graph.diversity <- function(graph, weights=NULL, vids=V(graph)) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  vids <- as.igraph.vs(graph, vids)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_diversity", graph, weights, vids-1,
        PACKAGE="igraph")
  if (getIgraphOpt("add.vertex.names") && is.named(graph)) { 
  names(res) <- get.vertex.attribute(graph, "name", vids) 
  }
  res
}

is.degree.sequence <- function(out.deg, in.deg=NULL) {
  # Argument checks
  out.deg <- as.numeric(out.deg)
  if (!is.null(in.deg)) in.deg <- as.numeric(in.deg)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_degree_sequence", out.deg, in.deg,
        PACKAGE="igraph")

  res
}

is.graphical.degree.sequence <- function(out.deg, in.deg=NULL) {
  # Argument checks
  out.deg <- as.numeric(out.deg)
  if (!is.null(in.deg)) in.deg <- as.numeric(in.deg)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_graphical_degree_sequence", out.deg, in.deg,
        PACKAGE="igraph")

  res
}

bipartite.projection.size <- function(graph, types=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  if (is.null(types) && "type" %in% list.vertex.attributes(graph)) { 
  types <- V(graph)$type 
  } 
  if (!is.null(types)) { 
  if (!is.logical(types)) { 
  warning("vertex types converted to logical") 
  } 
  types <- as.logical(types) 
  if (any(is.na(types))) { 
  stop("`NA' is not allowed in vertex types") 
  } 
  } else { 
  stop("Not a bipartite graph, supply `types' argument") 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_bipartite_projection_size", graph, types,
        PACKAGE="igraph")

  res
}

bipartite.mapping <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_bipartite", graph,
        PACKAGE="igraph")

  res
}

articulation.points <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_articulation_points", graph,
        PACKAGE="igraph")

  res
}

biconnected.components <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_biconnected_components", graph,
        PACKAGE="igraph")

  res
}

layout.star <- function(graph, center=V(graph)[1], order=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  center <- as.igraph.vs(graph, center)
  if (!is.null(order)) order <- as.numeric(order)-1

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_layout_star", graph, center-1, order,
        PACKAGE="igraph")

  res
}

layout.grid <- function(graph, width=0) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  width <- as.integer(width)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_layout_grid", graph, width,
        PACKAGE="igraph")

  res
}

layout.grid.3d <- function(graph, width=0, height=0) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  width <- as.integer(width)
  height <- as.integer(height)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_layout_grid_3d", graph, width, height,
        PACKAGE="igraph")

  res
}

layout.bipartite <- function(graph, types=NULL, hgap=1, vgap=1, maxiter=100) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  if (is.null(types) && "type" %in% list.vertex.attributes(graph)) { 
  types <- V(graph)$type 
  } 
  if (!is.null(types)) { 
  if (!is.logical(types)) { 
  warning("vertex types converted to logical") 
  } 
  types <- as.logical(types) 
  if (any(is.na(types))) { 
  stop("`NA' is not allowed in vertex types") 
  } 
  } else { 
  stop("Not a bipartite graph, supply `types' argument") 
  }
  hgap <- as.numeric(hgap)
  vgap <- as.numeric(vgap)
  maxiter <- as.integer(maxiter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_layout_bipartite", graph, types, hgap, vgap, maxiter,
        PACKAGE="igraph")

  res
}

similarity.jaccard <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total"), loops=FALSE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_similarity_jaccard", graph, vids-1, mode, loops,
        PACKAGE="igraph")

  res
}

similarity.dice <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total"), loops=FALSE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)
  loops <- as.logical(loops)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_similarity_dice", graph, vids-1, mode, loops,
        PACKAGE="igraph")

  res
}

similarity.invlogweighted <- function(graph, vids=V(graph), mode=c("all", "out", "in", "total")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2, "all"=3, "total"=3)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_similarity_inverse_log_weighted", graph, vids-1, mode,
        PACKAGE="igraph")

  res
}

community.le.to.membership <- function(merges, steps, membership) {
  # Argument checks
  merges <- as.matrix(structure(as.double(merges), dim=dim(merges)))
  steps <- as.integer(steps)
  membership <- as.numeric(membership)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_le_community_to_membership", merges, steps, membership,
        PACKAGE="igraph")

  res
}

mod.matrix <- function(graph, membership, weights=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  membership <- as.numeric(membership)-1
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_modularity_matrix", graph, membership, weights,
        PACKAGE="igraph")

  res
}

reindex.membership <- function(membership) {
  # Argument checks
  membership <- as.numeric(membership)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_reindex_membership", membership,
        PACKAGE="igraph")

  res
}

hrg.game <- function(hrg) {
  # Argument checks
  if (is.null(hrg)) { 
  hrg <- list(left=c(), right=c(), prob=c(), edges=c(), 
  vertices=c()) 
  } 
  hrg <- lapply(hrg[c("left","right","prob","edges","vertices")], 
  as.numeric)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_hrg_game", hrg,
        PACKAGE="igraph")

  res <- set.graph.attribute(res, 'name', 'Hierarchical random graph model')
  res
}

hrg.dendrogram <- function(hrg) {
  # Argument checks
  if (is.null(hrg)) { 
  hrg <- list(left=c(), right=c(), prob=c(), edges=c(), 
  vertices=c()) 
  } 
  hrg <- lapply(hrg[c("left","right","prob","edges","vertices")], 
  as.numeric)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_hrg_dendrogram", hrg,
        PACKAGE="igraph")

  res
}

hrg.consensus <- function(graph, hrg=NULL, start=FALSE, num.samples=10000) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  if (is.null(hrg)) { 
  hrg <- list(left=c(), right=c(), prob=c(), edges=c(), 
  vertices=c()) 
  } 
  hrg <- lapply(hrg[c("left","right","prob","edges","vertices")], 
  as.numeric)
  start <- as.logical(start)
  num.samples <- as.integer(num.samples)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_hrg_consensus", graph, hrg, start, num.samples,
        PACKAGE="igraph")

  res
}

hrg.create <- function(graph, prob) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  prob <- as.numeric(prob)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_hrg_create", graph, prob,
        PACKAGE="igraph")

  class(res) <- "igraphHRG"
  res
}

graphlets <- function(graph, weights=NULL, niter=1000) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  if (is.null(weights) && "weight" %in% list.edge.attributes(graph)) { 
  weights <- E(graph)$weight 
  } 
  if (!is.null(weights) && any(!is.na(weights))) { 
  weights <- as.numeric(weights) 
  } else { 
  weights <- NULL 
  }
  niter <- as.integer(niter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_graphlets", graph, weights, niter,
        PACKAGE="igraph")

  res
}

as.undirected <- function(graph, mode=c("collapse", "each", "mutual"), edge.attr.comb=getIgraphOpt("edge.attr.comb")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  mode <- switch(igraph.match.arg(mode), "collapse"=1, "each"=0, "mutual"=2)
  edge.attr.comb <- igraph.i.attribute.combination(edge.attr.comb)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_to_undirected", graph, mode, edge.attr.comb,
        PACKAGE="igraph")

  res
}

dyad.census <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_dyad_census", graph,
        PACKAGE="igraph")

  res
}

triad.census <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_triad_census", graph,
        PACKAGE="igraph")

  res
}

adjacent.triangles <- function(graph, vids=V(graph)) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  vids <- as.igraph.vs(graph, vids)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_adjacenct_triangles", graph, vids-1,
        PACKAGE="igraph")

  res
}

graph.maxflow <- function(graph, source, target, capacity=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  source <- as.igraph.vs(graph, source)
  target <- as.igraph.vs(graph, target)
  if (is.null(capacity) && "capacity" %in% list.edge.attributes(graph)) { 
  capacity <- E(graph)$capacity 
  } 
  if (!is.null(capacity) && any(!is.na(capacity))) { 
  capacity <- as.numeric(capacity) 
  } else { 
  capacity <- NULL 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_maxflow", graph, source-1, target-1, capacity,
        PACKAGE="igraph")

  res
}

dominator.tree <- function(graph, root, mode=c("out", "in")) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  root <- as.igraph.vs(graph, root)
  mode <- switch(igraph.match.arg(mode), "out"=1, "in"=2)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_dominator_tree", graph, root-1, mode,
        PACKAGE="igraph")

  res
}

stCuts <- function(graph, source, target) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  source <- as.igraph.vs(graph, source)
  target <- as.igraph.vs(graph, target)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_all_st_cuts", graph, source-1, target-1,
        PACKAGE="igraph")

  res
}

stMincuts <- function(graph, source, target, capacity=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  source <- as.igraph.vs(graph, source)
  target <- as.igraph.vs(graph, target)
  if (is.null(capacity) && "weight" %in% list.edge.attributes(graph)) { 
  capacity <- E(graph)$weight 
  } 
  if (!is.null(capacity) && any(!is.na(capacity))) { 
  capacity <- as.numeric(capacity) 
  } else { 
  capacity <- NULL 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_all_st_mincuts", graph, source-1, target-1, capacity,
        PACKAGE="igraph")

  res
}

is.separator <- function(graph, candidate) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  candidate <- as.igraph.vs(graph, candidate)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_separator", graph, candidate-1,
        PACKAGE="igraph")

  res
}

is.minimal.separator <- function(graph, candidate) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  candidate <- as.igraph.vs(graph, candidate)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_is_minimal_separator", graph, candidate-1,
        PACKAGE="igraph")

  res
}

minimal.st.separators <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_all_minimal_st_separators", graph,
        PACKAGE="igraph")

  res
}

minimum.size.separators <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_minimum_size_separators", graph,
        PACKAGE="igraph")

  res
}

graph.isoclass <- function(graph) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_isoclass", graph,
        PACKAGE="igraph")

  res
}

graph.isomorphic <- function(graph1, graph2) {
  # Argument checks
  if (!is.igraph(graph1)) { stop("Not a graph object") }
  if (!is.igraph(graph2)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_isomorphic", graph1, graph2,
        PACKAGE="igraph")

  res
}

graph.isocreate <- function(size, number, directed=TRUE) {
  # Argument checks
  size <- as.integer(size)
  number <- as.integer(number)
  directed <- as.logical(directed)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_isoclass_create", size, number, directed,
        PACKAGE="igraph")

  res
}

graph.isomorphic.vf2 <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is.igraph(graph1)) { stop("Not a graph object") }
  if (!is.igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) { 
  if ("color" %in% list.vertex.attributes(graph1)) { 
  vertex.color1 <- V(graph1)$color 
  } else { 
  vertex.color1 <- NULL 
  } 
  } 
  if (!is.null(vertex.color1)) { 
  vertex.color1 <- as.integer(vertex.color1)-1L 
  }
  if (missing(vertex.color2)) { 
  if ("color" %in% list.vertex.attributes(graph2)) { 
  vertex.color2 <- V(graph2)$color 
  } else { 
  vertex.color2 <- NULL 
  } 
  } 
  if (!is.null(vertex.color2)) { 
  vertex.color2 <- as.integer(vertex.color2)-1L 
  }
  if (missing(edge.color1)) { 
  if ("color" %in% list.edge.attributes(graph1)) { 
  edge.color1 <- E(graph1)$color 
  } else { 
  edge.color1 <- NULL 
  } 
  } 
  if (!is.null(edge.color1)) { 
  edge.color1 <- as.integer(edge.color1)-1L 
  }
  if (missing(edge.color2)) { 
  if ("color" %in% list.edge.attributes(graph2)) { 
  edge.color2 <- E(graph2)$color 
  } else { 
  edge.color2 <- NULL 
  } 
  } 
  if (!is.null(edge.color2)) { 
  edge.color2 <- as.integer(edge.color2)-1L 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_isomorphic_vf2", graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2,
        PACKAGE="igraph")

  res
}

graph.count.isomorphisms.vf2 <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is.igraph(graph1)) { stop("Not a graph object") }
  if (!is.igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) { 
  if ("color" %in% list.vertex.attributes(graph1)) { 
  vertex.color1 <- V(graph1)$color 
  } else { 
  vertex.color1 <- NULL 
  } 
  } 
  if (!is.null(vertex.color1)) { 
  vertex.color1 <- as.integer(vertex.color1)-1L 
  }
  if (missing(vertex.color2)) { 
  if ("color" %in% list.vertex.attributes(graph2)) { 
  vertex.color2 <- V(graph2)$color 
  } else { 
  vertex.color2 <- NULL 
  } 
  } 
  if (!is.null(vertex.color2)) { 
  vertex.color2 <- as.integer(vertex.color2)-1L 
  }
  if (missing(edge.color1)) { 
  if ("color" %in% list.edge.attributes(graph1)) { 
  edge.color1 <- E(graph1)$color 
  } else { 
  edge.color1 <- NULL 
  } 
  } 
  if (!is.null(edge.color1)) { 
  edge.color1 <- as.integer(edge.color1)-1L 
  }
  if (missing(edge.color2)) { 
  if ("color" %in% list.edge.attributes(graph2)) { 
  edge.color2 <- E(graph2)$color 
  } else { 
  edge.color2 <- NULL 
  } 
  } 
  if (!is.null(edge.color2)) { 
  edge.color2 <- as.integer(edge.color2)-1L 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_count_isomorphisms_vf2", graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2,
        PACKAGE="igraph")

  res
}

graph.subisomorphic.vf2 <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is.igraph(graph1)) { stop("Not a graph object") }
  if (!is.igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) { 
  if ("color" %in% list.vertex.attributes(graph1)) { 
  vertex.color1 <- V(graph1)$color 
  } else { 
  vertex.color1 <- NULL 
  } 
  } 
  if (!is.null(vertex.color1)) { 
  vertex.color1 <- as.integer(vertex.color1)-1L 
  }
  if (missing(vertex.color2)) { 
  if ("color" %in% list.vertex.attributes(graph2)) { 
  vertex.color2 <- V(graph2)$color 
  } else { 
  vertex.color2 <- NULL 
  } 
  } 
  if (!is.null(vertex.color2)) { 
  vertex.color2 <- as.integer(vertex.color2)-1L 
  }
  if (missing(edge.color1)) { 
  if ("color" %in% list.edge.attributes(graph1)) { 
  edge.color1 <- E(graph1)$color 
  } else { 
  edge.color1 <- NULL 
  } 
  } 
  if (!is.null(edge.color1)) { 
  edge.color1 <- as.integer(edge.color1)-1L 
  }
  if (missing(edge.color2)) { 
  if ("color" %in% list.edge.attributes(graph2)) { 
  edge.color2 <- E(graph2)$color 
  } else { 
  edge.color2 <- NULL 
  } 
  } 
  if (!is.null(edge.color2)) { 
  edge.color2 <- as.integer(edge.color2)-1L 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_subisomorphic_vf2", graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2,
        PACKAGE="igraph")

  res
}

graph.count.subisomorphisms.vf2 <- function(graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2) {
  # Argument checks
  if (!is.igraph(graph1)) { stop("Not a graph object") }
  if (!is.igraph(graph2)) { stop("Not a graph object") }
  if (missing(vertex.color1)) { 
  if ("color" %in% list.vertex.attributes(graph1)) { 
  vertex.color1 <- V(graph1)$color 
  } else { 
  vertex.color1 <- NULL 
  } 
  } 
  if (!is.null(vertex.color1)) { 
  vertex.color1 <- as.integer(vertex.color1)-1L 
  }
  if (missing(vertex.color2)) { 
  if ("color" %in% list.vertex.attributes(graph2)) { 
  vertex.color2 <- V(graph2)$color 
  } else { 
  vertex.color2 <- NULL 
  } 
  } 
  if (!is.null(vertex.color2)) { 
  vertex.color2 <- as.integer(vertex.color2)-1L 
  }
  if (missing(edge.color1)) { 
  if ("color" %in% list.edge.attributes(graph1)) { 
  edge.color1 <- E(graph1)$color 
  } else { 
  edge.color1 <- NULL 
  } 
  } 
  if (!is.null(edge.color1)) { 
  edge.color1 <- as.integer(edge.color1)-1L 
  }
  if (missing(edge.color2)) { 
  if ("color" %in% list.edge.attributes(graph2)) { 
  edge.color2 <- E(graph2)$color 
  } else { 
  edge.color2 <- NULL 
  } 
  } 
  if (!is.null(edge.color2)) { 
  edge.color2 <- as.integer(edge.color2)-1L 
  }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_count_subisomorphisms_vf2", graph1, graph2, vertex.color1, vertex.color2, edge.color1, edge.color2,
        PACKAGE="igraph")

  res
}

graph.isomorphic.34 <- function(graph1, graph2) {
  # Argument checks
  if (!is.igraph(graph1)) { stop("Not a graph object") }
  if (!is.igraph(graph2)) { stop("Not a graph object") }

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_isomorphic_34", graph1, graph2,
        PACKAGE="igraph")

  res
}

canonical.permutation <- function(graph, sh="fm") {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  sh <- switch(igraph.match.arg(sh), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_canonical_permutation", graph, sh,
        PACKAGE="igraph")

  res
}

permute.vertices <- function(graph, permutation) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  permutation <- as.numeric(permutation)-1

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_permute_vertices", graph, permutation,
        PACKAGE="igraph")

  res
}

graph.isomorphic.bliss <- function(graph1, graph2, sh1="fm", sh2="fm") {
  # Argument checks
  if (!is.igraph(graph1)) { stop("Not a graph object") }
  if (!is.igraph(graph2)) { stop("Not a graph object") }
  sh1 <- switch(igraph.match.arg(sh1), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)
  sh2 <- switch(igraph.match.arg(sh2), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_isomorphic_bliss", graph1, graph2, sh1, sh2,
        PACKAGE="igraph")

  res
}

graph.automorphisms <- function(graph, sh="fm") {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  sh <- switch(igraph.match.arg(sh), "f"=0, "fl"=1, "fs"=2, "fm"=3, "flm"=4, "fsm"=5)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_automorphisms", graph, sh,
        PACKAGE="igraph")

  res
}

scgNormEps <- function(V, groups, mtype=c("symmetric", "laplacian", "stochastic"), p=NULL, norm=c("row", "col")) {
  # Argument checks
  V <- as.matrix(structure(as.double(V), dim=dim(V)))
  groups <- as.numeric(groups)-1
  mtype <- switch(igraph.match.arg(mtype), "symmetric"=1, 
  "laplacian"=2, "stochastic"=3)
  if (!is.null(p)) p <- as.numeric(p)
  norm <- switch(igraph.match.arg(norm), "row"=1, "col"=2)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_scg_norm_eps", V, groups, mtype, p, norm,
        PACKAGE="igraph")

  res
}

graph.eigen <- function(graph, algorithm=c("arpack", "auto", "lapack", "comp_auto", "comp_lapack", "comp_arpack"), which=list(), options=igraph.arpack.default) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  algorithm <- switch(igraph.match.arg(algorithm), "auto"=0, "lapack"=1, 
  "arpack"=2, "comp_auto"=3, "comp_lapack"=4, 
  "comp_arpack"=5)
  which.tmp <- igraph.eigen.default; 
  which.tmp[ names(which) ] <- which ; which <- which.tmp
  options.tmp <- igraph.arpack.default; options.tmp[ names(options) ] <- options ; options <- options.tmp

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_eigen_adjacency", graph, algorithm, which, options,
        PACKAGE="igraph")

  res
}

power.law.fit.new <- function(data, xmin=-1, force.continuous=FALSE) {
  # Argument checks
  data <- as.numeric(data)
  xmin <- as.numeric(xmin)
  force.continuous <- as.logical(force.continuous)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_power_law_fit", data, xmin, force.continuous,
        PACKAGE="igraph")

  res
}

sir <- function(graph, beta, gamma, no.sim=100) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  beta <- as.numeric(beta)
  gamma <- as.numeric(gamma)
  no.sim <- as.integer(no.sim)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_sir", graph, beta, gamma, no.sim,
        PACKAGE="igraph")

  class(res) <- "sir"
  res
}

convex.hull <- function(data) {
  # Argument checks
  data <- as.matrix(structure(as.double(data), dim=dim(data)))

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_convex_hull", data,
        PACKAGE="igraph")

  res
}

revolver.ml.d <- function(graph, niter, delta=1e-10, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  niter <- as.integer(niter)
  delta <- as.numeric(delta)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_d", graph, niter, delta, filter,
        PACKAGE="igraph")

  res
}

revolver.probs.d <- function(graph, kernel, ntk=FALSE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  kernel <- as.numeric(kernel)
  ntk <- as.logical(ntk)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_probs_d", graph, kernel, ntk,
        PACKAGE="igraph")

  res
}

revolver.ml.de <- function(graph, niter, cats, delta=1e-10, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  niter <- as.integer(niter)
  cats <- as.numeric(cats)
  delta <- as.numeric(delta)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_de", graph, niter, cats, delta, filter,
        PACKAGE="igraph")

  res
}

revolver.probs.de <- function(graph, kernel, cats) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  kernel <- as.matrix(structure(as.double(kernel), dim=dim(kernel)))
  cats <- as.numeric(cats)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_probs_de", graph, kernel, cats,
        PACKAGE="igraph")

  res
}

revolver.ml.ade <- function(graph, niter, cats, agebins=300, delta=1e-10, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  niter <- as.integer(niter)
  cats <- as.numeric(cats)
  agebins <- as.integer(agebins)
  delta <- as.numeric(delta)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_ade", graph, niter, cats, agebins, delta, filter,
        PACKAGE="igraph")

  res
}

revolver.probs.ade <- function(graph, kernel, cats) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  kernel <- structure(as.double(kernel), dim=dim(kernel))
  cats <- as.numeric(cats)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_probs_ade", graph, kernel, cats,
        PACKAGE="igraph")

  res
}

revolver.ml.f <- function(graph, niter, delta=1e-10) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  niter <- as.integer(niter)
  delta <- as.numeric(delta)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_f", graph, niter, delta,
        PACKAGE="igraph")

  res
}

revolver.ml.df <- function(graph, niter, delta=1e-10) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  niter <- as.integer(niter)
  delta <- as.numeric(delta)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_df", graph, niter, delta,
        PACKAGE="igraph")

  res
}

revolver.ml.l <- function(graph, niter, agebins=300, delta=1e-10) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  niter <- as.integer(niter)
  agebins <- as.integer(agebins)
  delta <- as.numeric(delta)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_l", graph, niter, agebins, delta,
        PACKAGE="igraph")

  res
}

revolver.ml.ad <- function(graph, niter, agebins=300, delta=1e-10, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  niter <- as.integer(niter)
  agebins <- as.integer(agebins)
  delta <- as.numeric(delta)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_ad", graph, niter, agebins, delta, filter,
        PACKAGE="igraph")

  res
}

revolver.probs.ad <- function(graph, kernel, ntk=FALSE) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  kernel <- as.matrix(structure(as.double(kernel), dim=dim(kernel)))
  ntk <- as.logical(ntk)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_probs_ad", graph, kernel, ntk,
        PACKAGE="igraph")

  res
}

revolver.ml.D.alpha <- function(graph, alpha, abstol=1e-8, reltol=1e-8, maxit=1000, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  alpha <- as.numeric(alpha)
  abstol <- as.numeric(abstol)
  reltol <- as.numeric(reltol)
  maxit <- as.integer(maxit)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_D_alpha", graph, alpha, abstol, reltol, maxit, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.D.alpha.a <- function(graph, alpha, a, abstol=1e-8, reltol=1e-8, maxit=1000, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  abstol <- as.numeric(abstol)
  reltol <- as.numeric(reltol)
  maxit <- as.integer(maxit)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_D_alpha_a", graph, alpha, a, abstol, reltol, maxit, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.DE.alpha.a <- function(graph, cats, alpha, a, coeffs, abstol=1e-8, reltol=1e-8, maxit=1000, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  cats <- as.numeric(cats)
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  coeffs <- as.numeric(coeffs)
  abstol <- as.numeric(abstol)
  reltol <- as.numeric(reltol)
  maxit <- as.integer(maxit)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_DE_alpha_a", graph, cats, alpha, a, coeffs, abstol, reltol, maxit, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.AD.alpha.a.beta <- function(graph, alpha, a, beta, abstol=1e-8, reltol=1e-8, maxit=1000, agebins=300, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  beta <- as.numeric(beta)
  abstol <- as.numeric(abstol)
  reltol <- as.numeric(reltol)
  maxit <- as.integer(maxit)
  agebins <- as.integer(agebins)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_AD_alpha_a_beta", graph, alpha, a, beta, abstol, reltol, maxit, agebins, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.AD.dpareto <- function(graph, alpha, a, paralpha, parbeta, parscale, abstol=1e-8, reltol=1e-8, maxit=1000, agebins=300, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  paralpha <- as.numeric(paralpha)
  parbeta <- as.numeric(parbeta)
  parscale <- as.numeric(parscale)
  abstol <- as.numeric(abstol)
  reltol <- as.numeric(reltol)
  maxit <- as.integer(maxit)
  agebins <- as.integer(agebins)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_AD_dpareto", graph, alpha, a, paralpha, parbeta, parscale, abstol, reltol, maxit, agebins, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.AD.dpareto.eval <- function(graph, alpha, a, paralpha, parbeta, parscale, agebins=300, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  paralpha <- as.numeric(paralpha)
  parbeta <- as.numeric(parbeta)
  parscale <- as.numeric(parscale)
  agebins <- as.integer(agebins)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_AD_dpareto_eval", graph, alpha, a, paralpha, parbeta, parscale, agebins, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.ADE.alpha.a.beta <- function(graph, cats, alpha, a, beta, coeffs, abstol=1e-8, reltol=1e-8, maxit=1000, agebins=300, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  cats <- as.numeric(cats)
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  beta <- as.numeric(beta)
  coeffs <- as.numeric(coeffs)
  abstol <- as.numeric(abstol)
  reltol <- as.numeric(reltol)
  maxit <- as.integer(maxit)
  agebins <- as.integer(agebins)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_ADE_alpha_a_beta", graph, cats, alpha, a, beta, coeffs, abstol, reltol, maxit, agebins, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.ADE.dpareto <- function(graph, cats, alpha, a, paralpha, parbeta, parscale, coeffs, abstol=1e-8, reltol=1e-8, maxit=1000, agebins=300, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  cats <- as.numeric(cats)
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  paralpha <- as.numeric(paralpha)
  parbeta <- as.numeric(parbeta)
  parscale <- as.numeric(parscale)
  coeffs <- as.numeric(coeffs)
  abstol <- as.numeric(abstol)
  reltol <- as.numeric(reltol)
  maxit <- as.integer(maxit)
  agebins <- as.integer(agebins)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_ADE_dpareto", graph, cats, alpha, a, paralpha, parbeta, parscale, coeffs, abstol, reltol, maxit, agebins, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.ADE.dpareto.eval <- function(graph, cats, alpha, a, paralpha, parbeta, parscale, coeffs, agebins=300, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  cats <- as.numeric(cats)
  alpha <- as.numeric(alpha)
  a <- as.numeric(a)
  paralpha <- as.numeric(paralpha)
  parbeta <- as.numeric(parbeta)
  parscale <- as.numeric(parscale)
  coeffs <- as.numeric(coeffs)
  agebins <- as.integer(agebins)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_ADE_dpareto_eval", graph, cats, alpha, a, paralpha, parbeta, parscale, coeffs, agebins, filter,
        PACKAGE="igraph")

  res
}

revolver.ml.ADE.dpareto.evalf <- function(graph, cats, par, agebins, filter=NULL) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  cats <- as.numeric(cats)
  par <- as.matrix(structure(as.double(par), dim=dim(par)))
  agebins <- as.integer(agebins)
  if (!is.null(filter)) filter <- as.numeric(filter)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_ml_ADE_dpareto_evalf", graph, cats, par, agebins, filter,
        PACKAGE="igraph")

  res
}

revolver.probs.ADE.dpareto <- function(graph, par, cats, gcats, agebins) {
  # Argument checks
  if (!is.igraph(graph)) { stop("Not a graph object") }
  par <- as.matrix(structure(as.double(par), dim=dim(par)))
  cats <- as.numeric(cats)
  gcats <- as.numeric(gcats)
  agebins <- as.integer(agebins)

  on.exit( .Call("R_igraph_finalizer", PACKAGE="igraph") )
  # Function call
  res <- .Call("R_igraph_revolver_probs_ADE_dpareto", graph, par, cats, gcats, agebins,
        PACKAGE="igraph")

  res
}

