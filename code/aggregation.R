rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

algo <- "KD-IC-NML"

# compute the parsing order of the aggregate nodes
leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

res <- sapply(agg_nodes, function(agg_node){
  vec <- distances(itree, agg_node, leaves, mode = "out")
  max( vec[which(vec!=Inf)])
})

ordered_aggnodes_names <- names(sort(res))

for(aggnode_name in ordered_aggnodes_names){
  
  i_node <- which(V(itree)$name == aggnode_name)
  children <- ego(itree, order = 1, nodes = i_node, mode = "out")[[1]][-1]
  
  # estimate empirical copula
  
    # load data for all children
    # compute ranks
    # save ranks in matrix
  
  # permutate the associated samples 
  
}


getPerm <- function(idseries){
  
  # identify the children
  # save rank matrix (with each colnames with the right name)
}


# for each node, run myperm(idseries)

myperm <- function(idseries){
  
  # load the rank matrix 
  
  # hierarchical permutations of the children

}

# Generate samples for all bottom nodes

# for each aggregate nodes
  # compute the copula (permutations) 
  # permutate the tows
  # compute the sum