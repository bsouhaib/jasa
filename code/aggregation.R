rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

##########
# compute the parsing order of the aggregate nodes
leaves <- V(itree)[degree(itree, mode="out") == 0]
agg_nodes <- V(itree)[degree(itree, mode="out") != 0]

depth_aggnodes <- sapply(agg_nodes, function(agg_node){
  vec <- distances(itree, agg_node, leaves, mode = "out")
  max( vec[which(vec!=Inf)])
})

ordered_agg_nodes_names <- names(sort(depth_aggnodes))
ordered_agg_nodes <- V(itree)[match(ordered_agg_nodes_names, V(itree)$name)]
##########

alliseries <- seq(length(bottomSeries))
algo <- "KD-IC-NML"

ntest <- length(test$id)
allidtest <- seq(ntest)


# Generate samples
for(idtest in allidtest){
  iday <- getInfo(idtest)$iday
  hour <- getInfo(idtest)$hour
  
  Q <- matrix(NA, nrow = M, ncol = length(alliseries))
  colnames(Q) <- bottomSeries[alliseries]
  for(j in seq_along(alliseries)){
    if(j%%100 == 0)
    print(j)
    
    iseries <- alliseries[j]
    idseries <- bottomSeries[iseries]
    
    res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
    load(res_file)
    if(algo == "Uncond" || algo == "PeriodOfDay"){
      invcdf <- approxfun(alphas, qFtest[, idtest], rule = 2)
    }else if(algo == "TBATS"){	
      invcdf <- approxfun(alphas, all_qf[[iday]][, hour], rule = 2)
    }else if(algo == "KD-IC-NML"){	
      if(hour %in% hours_night){
        index <- match(hour, hours_night)
        qtauhat <- res_testing$res_nighthours[[iday]][[index]]$qtauhat
        tauhat <- res_testing$res_nighthours[[iday]][[index]]$tauhat
      }else{
        index <- match(hour, hours_day)
        qtauhat <- res_testing$res_dayhours[[iday]][[index]]$qtauhat
        tauhat <- res_testing$res_dayhours[[iday]][[index]]$tauhat
      }
      invcdf <- approxfun(tauhat, qtauhat, rule = 2)
    }else{
      stop("error")
    }
    
    Q[, j] <- invcdf(q_probs)
  }# series
  
  # rank_X <- apply(Q, 2, rank, ties.method = "random")
  # I know that the rank of each observations is 1 --> M
  
  variables <- colnames(Q)
  # PERM-BU
  for(inode in seq_along(ordered_agg_nodes)){
    agg_node <- ordered_agg_nodes[inode]
    idseries_agg <- names(agg_node)
    children_nodes <- ego(itree, order = 1, nodes = agg_node, mode = "out")[[1]][-1]
    
    # load permutation file
    perm_file <- file.path(permutations.folder, paste("perm_", idseries_agg, ".Rdata", sep = "")) 
    load(perm_file) # c("list_permutations", "list_ties")
    
    perm_idtest <- list_permutations[[hour]]
    stopifnot(all(colnames(perm_idtest) == names(children_nodes)))
    
    depth_node <- depth_aggnodes[match(idseries_agg, names(depth_aggnodes))]
    if(depth_node == 1){
      
      #Q[, id_cols] <- Q[perm_idtest, id_cols]
      
    }else{
      
    }  
     
    # permutate the associated samples 
    match(colnames(perm_idtest), variables)
    

    browser() 
  }
  
  # NAIVE-BU
  
  # if(NAIVE-BU) Shuffle the samples 
  
  # SAVE MARGINAL TOO ?
  
  # Sum the samples
  # samples_agg <- tcrossprod(Sagg, samples)
  
  # Compute quantile scores and save them
  
  
  stop("done")
  
  
}




