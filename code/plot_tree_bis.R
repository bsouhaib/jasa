second_tree <- itree
all_ids <- all_nkids <- NULL
myvsize <- numeric(length(V(itree)))
for(inode in seq_along(ordered_agg_nodes)){
  #print(inode)
  
  agg_node <- ordered_agg_nodes[inode]
  idseries_agg <- names(agg_node)
  iagg <- match(idseries_agg, aggSeries)
  children_nodes <- ego(itree, order = 1, nodes = agg_node, mode = "out")[[1]][-1]
  nkids <- length(children_nodes)
  
  #print(nkids)
  id <- as.numeric(which(V(itree) == agg_node))
  all_ids <- c(all_ids, id)
  all_nkids <- c(all_nkids, nkids)
}
myvsize[all_ids] <- 7
myvsize[-all_ids] <- 0

second_tree <- set.vertex.attribute(second_tree, "name", all_ids, all_nkids)
second_tree <- set.vertex.attribute(second_tree, "name", setdiff(seq(length(V(itree))), all_ids), "")

plot(second_tree, layout = layout.reingold.tilford(second_tree, root=1, circular=T), vertex.size=myvsize, edge.arrow.size=0)
