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
algo.bottom <- algo <- "KD-IC-NML"
algo.agg <- "TBATS"

ntest <- length(test$id)
allidtest <- seq(ntest)

list_results_perm <- list_results_naive <- vector("list", ntest)

# Generate samples
for(idtest in allidtest){
  print(idtest)
  print(base::date())
  
  res_byidtest_file <- file.path(basef.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
  load(res_byidtest_file)
  # "QF_agg_idtest", "QF_bottom_idtest", "PROB_bottom_idtest"
  
  iday <- getInfo(idtest)$iday
  hour <- getInfo(idtest)$hour
  
  Q <- matrix(NA, nrow = M, ncol = length(alliseries))
  colnames(Q) <- bottomSeries[alliseries]
  for(j in seq_along(alliseries)){
    #if(j%%100 == 0)
    #print(j)
    
    iseries <- alliseries[j]
    idseries <- bottomSeries[iseries]
    
    #res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
    #load(res_file)
    
    if(algo == "Uncond" || algo == "PeriodOfDay"){
      invcdf <- approxfun(alphas, qFtest[, idtest], rule = 2)
    }else if(algo == "TBATS"){	
      
      #invcdf <- approxfun(alphas, all_qf[[iday]][, hour], rule = 2)
    
    }else if(algo == "KD-IC-NML"){	
      
      invcdf <- approxfun(PROB_bottom_idtest[, j], QF_bottom_idtest[, j], rule = 2)
    
    #  if(hour %in% hours_night){
    #    index <- match(hour, hours_night)
    #    qtauhat <- res_testing$res_nighthours[[iday]][[index]]$qtauhat
    #    tauhat <- res_testing$res_nighthours[[iday]][[index]]$tauhat
    #  }else{
    #    index <- match(hour, hours_day)
    #    qtauhat <- res_testing$res_dayhours[[iday]][[index]]$qtauhat
    #    tauhat <- res_testing$res_dayhours[[iday]][[index]]$tauhat
    #  }
    #  invcdf <- approxfun(tauhat, qtauhat, rule = 2)
    }else{
      stop("error")
    }
    
    Q[, j] <- invcdf(q_probs)
  }# series
  
  # rank_X <- apply(Q, 2, rank, ties.method = "random")
  # I know that the rank of each observations is 1 --> M
  samples <- Q
  variables <- colnames(samples)
  
  mat_test <- NULL
  # PERM-BU
  for(inode in seq_along(ordered_agg_nodes)){
    
    agg_node <- ordered_agg_nodes[inode]
    idseries_agg <- names(agg_node)
    iagg <- match(idseries_agg, aggSeries)
    children_nodes <- ego(itree, order = 1, nodes = agg_node, mode = "out")[[1]][-1]
    nkids <- length(children_nodes)
    
    # load permutation file
    perm_file <- file.path(permutations.folder, paste("perm_", idseries_agg, ".Rdata", sep = "")) 
    load(perm_file) # c("list_permutations", "list_ties")
    
    ranks_historical <- list_permutations[[hour]]
    stopifnot(all(colnames(ranks_historical) == names(children_nodes)))
    
    depth_node <- depth_aggnodes[match(idseries_agg, names(depth_aggnodes))]
    
    samples_children <- matrix(NA, nrow = M, ncol = nkids)
    
    columns_agg <- which(children_nodes %in% agg_nodes)
    columns_bottom <- which(children_nodes %in% leaves)
    children_names <- names(children_nodes)
    
    # Extracting/computing the samples for each child
    if(length(columns_agg) > 0){
      id_agg_children  <- match(children_names[columns_agg], aggSeries)
      samples_agg_children <- t(tcrossprod(Sagg[id_agg_children, , drop = F], samples))
      samples_children[, columns_agg] <- samples_agg_children
    }
    
    if(length(columns_bottom) > 0){
      id_bottom_children  <- match(children_names[columns_bottom], bottomSeries)
      samples_children[, columns_bottom] <- samples[, id_bottom_children]
    }
    
    # Computing the ranks of the samples for each child
    ranks_samples_children <- sapply(seq(ncol(samples_children)), function(j){
      rank(samples_children[, j], ties.method = "random")
    })
    
    index_mat <- sapply(seq(nkids), function(j){
      res <- match(ranks_historical[, j], ranks_samples_children[, j])
      stopifnot(all(!is.na(res)))
      res
    })
    
    # Permutating the rows
    if(length(columns_bottom) > 0){
      samples[, id_bottom_children] <- sapply(seq_along(id_bottom_children), function(j){
        samples[index_mat[, columns_bottom[j]], id_bottom_children[j]]
      })
    }
    
    if(length(columns_agg) > 0){
      res <- lapply(seq_along(id_agg_children), function(j){
        id <- which(Sagg[id_agg_children[j], ] == 1)
        #print(id)
        #print("---")
        samples[index_mat[, columns_agg[j]], id, drop = F]
      })
      ids <- lapply(id_agg_children, function(id_agg_child){
        which(Sagg[id_agg_child, ] == 1)
      })
      ids <- unlist(ids)
      samples[, ids] <- do.call(cbind, res)
    }

  }# agg node
  
  # PERM-BU
  list_results_perm[[idtest]] <- samples_agg <- t(tcrossprod(Sagg, samples))
  
  # NAIVE-BU - shuffle the samples 
  list_results_naive[[idtest]] <- samples_agg_naivebu <- t(tcrossprod(Sagg, apply(Q, 2, sample)))
  stop("done")
}

# load data for all aggregates - obs_agg_idtest
# Compute quantile scores and save them



