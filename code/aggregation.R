rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

# Generate samples


# compute the parsing order of the aggregate nodes
leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

res <- sapply(agg_nodes, function(agg_node){
  vec <- distances(itree, agg_node, leaves, mode = "out")
  max( vec[which(vec!=Inf)])
})

ordered_agg_nodes_names <- names(sort(res))
ordered_agg_nodes <- V(itree)[match(ordered_agg_nodes_names, V(itree)$name)]

stop("done")

for(agg_node in ordered_agg_nodes){
  
  # load permutation file
  
  # permutate the associated samples 
  
}

