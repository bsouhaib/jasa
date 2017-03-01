
leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

depth_aggnodes <- sapply(agg_nodes, function(agg_node){
  vec <- distances(itree, agg_node, leaves, mode = "out")
  max( vec[which(vec!=Inf)])
})

ordered_agg_nodes_names <- names(sort(depth_aggnodes))
ordered_agg_nodes <- V(itree)[match(ordered_agg_nodes_names, V(itree)$name)]

load("samples_agg_DYNREG_KD-IC-NML_1.Rdata")
D <-list_samples_agg[[1]]


apply(Sagg, 1, sum)

inode <- 1
agg_node <- ordered_agg_nodes[inode]
idseries_agg <- names(agg_node)
iagg <- match(idseries_agg, aggSeries)
children_nodes <- ego(itree, order = 1, nodes = agg_node, mode = "out")[[1]][-1]
nkids <- length(children_nodes)

ERROR !!

v <- D[, which(Sagg[iagg, ] == 1), 1]
agg_bu <- apply(v, 1, sum)

agg_v <- D[, iagg, 1]

