rm(list = ls())
args = (commandArgs(TRUE))
if(length(args) == 0){
  idjob <- 1
  allidtest <- 1:4 #1:1104
}else{
  
  for(i in 1:length(args)){
    print(args[[i]])
  }
  
  idjob <- as.numeric(args[[1]])
  allidtest <- NULL
  for(i in seq(2, length(args))){
    allidtest <- c(allidtest, as.numeric(args[[i]]))
  }
}
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)

set.seed(1986)

compute_crps <- function(methods, n, mat_samples, observations){
  res <- sapply(seq_along(methods), function(imethod){
    sapply(seq(n), function(i){
      crps_sampling(mat_samples[, i, imethod], observations[i])
    })
  })
  colnames(res) <- methods
  res
}

compute_qscores <- function(methods, n, mat_samples, observations){
  sorted_samples <- apply(mat_samples, c(2, 3), sort)
  qscores <- sapply(seq(n), function(i){
    obs <- observations[i]
    sapply(seq_along(methods), function(imethod){
      sapply(seq(length(q_probs)), function(iprob){
        qf <- sorted_samples[iprob, i, imethod]
        2 * ((obs <= qf) - q_probs[iprob]) * (qf - obs)
      })
    })
  }, simplify = 'array')
  qscores
}


load(file.path(work.folder, "myinfo.Rdata"))
n_bottom <- length(bottomSeries)

algo.bottom  <- "KD-IC-NML"
algo.agg <- "TBATS"

covmethod <- c("shrink")
W1file <- file.path(work.folder, "wmatrices", paste("W1_", algo.agg, "_", algo.bottom, "_", covmethod, ".Rdata", sep = "")) 
load(W1file)
J <- Matrix(cbind(matrix(0, nrow = n_bottom, ncol = n_agg), diag(n_bottom)), sparse = TRUE)
U <- Matrix(rbind(diag(n_agg), -t(Sagg)), sparse = TRUE)

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

#alliseries <- seq(length(bottomSeries))
ntest <- length(test$id)
#allidtest <- seq(ntest)
#list_results_agg_perm <- list_results_agg_naive <- list_results_agg_base <- list_obs_agg  <- vector("list", ntest)

list_crps_agg <- list_crps_bot <- list_qscores_agg <- vector("list", ntest)
sum_overtest_qscores_agg <- sum_overtest_qscores_bot <- 0
# Generate samples
for(idtest in allidtest){
  print(idtest)
  #if(idtest%%48 == 0)
  #{ 
  #  print(idtest)
  #  print(base::date())
  #}
  
  res_byidtest_file <- file.path(basef.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
  load(res_byidtest_file)
  # "QF_agg_idtest", "QF_bottom_idtest", "PROB_bottom_idtest" "obs_agg_idtest", "obs_bottom_idtest"
  
  iday <- getInfo(idtest)$iday
  hour <- getInfo(idtest)$hour
  
  #Q <- matrix(NA, nrow = M, ncol = length(alliseries))
  #colnames(Q) <- bottomSeries[alliseries]

  for(do.agg in c(TRUE, FALSE)){
    if(do.agg){
      set_series <- aggSeries
      algo <- algo.agg
      base_samples_agg <- matrix(NA, nrow = M, ncol = length(set_series))
      colnames(base_samples_agg) <- set_series
    }else{
      set_series <- bottomSeries
      algo <- algo.bottom
      base_samples_bottom <- matrix(NA, nrow = M, ncol = length(set_series))
      colnames(base_samples_bottom) <- set_series
    }
    for(j in seq_along(set_series)){
      #if(j%%100 == 0)
      #print(j)
      
      #iseries <- alliseries[j]
      idseries <- set_series[j]
      
      #res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
      #load(res_file)
      
      if(algo == "Uncond" || algo == "PeriodOfDay"){
        #invcdf <- approxfun(alphas, qFtest[, idtest], rule = 2)
      }else if(algo == "TBATS"){	
        invcdf <- approxfun(alphas, QF_agg_idtest[, j], rule = 2)
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
      
      if(do.agg){
        base_samples_agg[, j]    <- invcdf(q_probs)
      }else{
        base_samples_bottom[, j] <- invcdf(q_probs)
      }
      #Q[, j] <- invcdf(q_probs)
    }# series
    
  }# agg and bottom
  
  # rank_X <- apply(Q, 2, rank, ties.method = "random")
  # I know that the rank of each observations is 1 --> M
  perm_samples_bottom <- base_samples_bottom
  variables <- colnames(perm_samples_bottom)

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
      samples_agg_children <- t(tcrossprod(Sagg[id_agg_children, , drop = F], perm_samples_bottom))
      samples_children[, columns_agg] <- samples_agg_children
    }
    
    if(length(columns_bottom) > 0){
      id_bottom_children  <- match(children_names[columns_bottom], bottomSeries)
      samples_children[, columns_bottom] <- perm_samples_bottom[, id_bottom_children]
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
      perm_samples_bottom[, id_bottom_children] <- sapply(seq_along(id_bottom_children), function(j){
        perm_samples_bottom[index_mat[, columns_bottom[j]], id_bottom_children[j]]
      })
    }
    
    if(length(columns_agg) > 0){
      res <- lapply(seq_along(id_agg_children), function(j){
        id <- which(Sagg[id_agg_children[j], ] == 1)
        #print(id)
        #print("---")
        perm_samples_bottom[index_mat[, columns_agg[j]], id, drop = F]
      })
      ids <- lapply(id_agg_children, function(id_agg_child){
        which(Sagg[id_agg_child, ] == 1)
      })
      ids <- unlist(ids)
      perm_samples_bottom[, ids] <- do.call(cbind, res)
    }

  }# agg node
  
  # PERM-BU
  #list_results_agg_perm[[idtest]] <- perm_samples_agg <- t(tcrossprod(Sagg, perm_samples_bottom))
  
  # NAIVE-BU - shuffle the samples 
  #list_results_agg_naive[[idtest]] <- naivebu_samples_agg <- t(tcrossprod(Sagg, apply(base_samples_bottom, 2, sample)))
  
  # BASE
  #list_results_agg_base[[idtest]] <- base_samples_agg
  
  #list_obs_agg[[idtest]] <- obs_agg_idtest
  
  ###### MINT ############
  #  adjustments 
  b_hat <- apply(base_samples_bottom, 2, mean)
  a_hat <- apply(base_samples_agg, 2, mean)
  y_hat <- c(a_hat, b_hat)
  adj_bottom <- mint_betastar(W1, y_hat = y_hat)
  adj_agg    <- Sagg %*% adj_bottom
  
  adj_bottom <- as.numeric(adj_bottom)
  adj_agg    <-  as.numeric(adj_agg)
  ########################
  
  agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "NAIVEBU-MINT", "PERMBU-MINT")
  samples_agg <- array(NA, c(M, n_agg, length(agg_methods)))
  samples_agg[, , match("BASE", agg_methods)] <- base_samples_agg
  samples_agg[, , match("NAIVEBU", agg_methods)] <- t(tcrossprod(Sagg, apply(base_samples_bottom, 2, sample)))
  samples_agg[, , match("PERMBU", agg_methods)] <- t(tcrossprod(Sagg, perm_samples_bottom))
  
  samples_agg[, , match("NAIVEBU-MINT", agg_methods)] <- t(t(samples_agg[, , match("NAIVEBU", agg_methods)]) + adj_agg)
  samples_agg[, , match("PERMBU-MINT", agg_methods)]  <- t(t(samples_agg[, , match("PERMBU" , agg_methods)]) + adj_agg)
  
  # CRPS
  aggmethods_crps <- compute_crps(agg_methods, n_agg, samples_agg, obs_agg_idtest)
  list_crps_agg[[idtest]] <- aggmethods_crps
  
  # QS
  qscores_agg <- compute_qscores(agg_methods, n_agg, samples_agg, obs_agg_idtest)
  sum_overtest_qscores_agg <- sum_overtest_qscores_agg + qscores_agg
    
  ### BOTTOM
  bot_methods <- c("BASE", "BASE-MINT")
  samples_bot <- array(NA, c(M, n_bottom, length(bot_methods)))
  samples_bot[, , match("BASE", bot_methods)] <- base_samples_bottom
  samples_bot[, , match("BASE-MINT", bot_methods)] <- t(t(base_samples_bottom) + adj_bottom)
  
  # CRPS
  botmethods_crps <- compute_crps(bot_methods, n_bottom, samples_bot, obs_bottom_idtest)
  list_crps_bot[[idtest]] <- botmethods_crps
  
  # QS
  qscores_bottom <- compute_qscores(bot_methods, n_bottom, samples_bot, obs_bottom_idtest)
  sum_overtest_qscores_bot <- sum_overtest_qscores_bot + qscores_bottom
}

avg_qscores_agg <- sum_overtest_qscores_agg/length(allidtest)
avg_qscores_bot <- sum_overtest_qscores_bot/length(allidtest)

res_job <- file.path(loss.folder, "HTS", paste("results_HTS_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
save(file = res_job, list = c("list_crps_agg", "list_crps_bot", "avg_qscores_agg", "avg_qscores_bot"))

stop("FINISHED")



##############################################
if(FALSE){
  # load data for all aggregates - obs_agg_idtest
  # Compute quantile scores and save them
  savepdf(file.path(results.folder, paste("CDFPLOT", sep = "") ))
  par(mfrow = c(2, 2))
  for(iagg in seq(n_agg)){
    for(idtest in seq(4)){
      MAT <- cbind(sort(list_results_agg_perm[[idtest]][, iagg]), 
                   sort(list_results_agg_naive[[idtest]][, iagg]),
                   sort(list_results_agg_base[[idtest]][, iagg]),
                   list_obs_agg[[idtest]][iagg])
    matplot(x = MAT, y = seq(1, M)/M, pch = 1, cex = .5)
    #abline(v = list_obs_agg[[idtest]][1])
    }
  }
  dev.off()
}

iagg <- 10
v <- lapply(seq(393), function(idtest){
  list_qscores_agg[[idtest]][, , iagg]
})
vv <- Reduce("+", v)/393
matplot(x = seq(1, M)/M, y = vv, col = c("black", "red", "blue", "orange", "darkblue"))
abline(v = 0.5)


temp <- Reduce("+", list_qscores_agg[seq(300)])


#list_qscores_agg[[idtest]] <- qscores

#comparing with CRPS
#res2 <- apply(qscores, c(2, 3), sum)/length(q_probs)

par(mfrow = c(2, 2))
for(iagg in c(1, 10)){
  matplot(y = qscores[, , iagg], x = seq(1, M)/M, pch = 1, cex = .5, col = c("black", "red", "blue", "orange", "darkblue"))
  MAT <- cbind(sorted_samples_agg[, iagg,], obs_agg_idtest[iagg])
  matplot(x = MAT, y = seq(1, M)/M, pch = 1, cex = .5, col = c("black", "red", "blue", "orange", "darkblue"))
}

par(mfrow = c(2, 2))
for(i in c(100)){
  matplot(y = sum_overtest_qscores_bot[, , i], x = seq(1, M)/M, pch = 1, cex = .5, col = c("black", "magenta"))
  sorted_samples_bot <- apply(samples_bot, c(2, 3), sort)
  MAT <- cbind(sorted_samples_bot[, i,], obs_bottom_idtest[i])
  matplot(x = MAT, y = seq(1, M)/M, pch = 1, cex = .5, col = c("black", "magenta"))
}
