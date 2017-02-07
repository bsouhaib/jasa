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
library(Matrix)

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
algo.agg <- "DYNREG"

bot_methods <- c("BASE", "BASE-MINT", "BASE-MEANCOMB")
agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MEANCOMB")

#bot_methods <- c("BASE")
#agg_methods <- c("BASE", "NAIVEBU", "PERMBU")

do.mint <- TRUE
if(do.mint){
  covmethod <- c("shrink")
  W1file <- file.path(work.folder, "wmatrices", paste("W1_", algo.agg, "_", algo.bottom, "_", covmethod, ".Rdata", sep = "")) 
  load(W1file)
  J <- Matrix(cbind(matrix(0, nrow = n_bottom, ncol = n_agg), diag(n_bottom)), sparse = TRUE)
  U <- Matrix(rbind(diag(n_agg), -t(Sagg)), sparse = TRUE)
}

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

list_crps_agg <- list_crps_bot <- list_qscores_agg <- list_mse_agg <- list_mse_bot  <- vector("list", ntest)
list_samples_agg <- vector("list", ntest)
sum_overtest_qscores_agg <- sum_overtest_qscores_bot <- 0

# Generate samples
for(idtest in allidtest){
  print(idtest)
  print(base::date())
  #if(idtest%%48 == 0)
  #{ 
  #  print(idtest)
  #  print(base::date())
  #}
  
  res_byidtest_file <- file.path(work.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
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
        #
      }else if(algo == "DYNREG"){	
        invcdf <- approxfun(taus, QF_agg_idtest[, j], rule = 2)
      }else if(algo == "KD-IC-NML"){	
        invcdf <- approxfun(taus, QF_bottom_idtest[, j], rule = 2)
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
    
    ranks_historical <- mat_permutations
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
  if(do.mint){
    #b_hat <- apply(base_samples_bottom, 2, mean)
    #a_hat <- apply(base_samples_agg, 2, mean)
    b_hat <- mean_bottom_idtest
    a_hat <- mean_agg_idtest 
      
    y_hat <- c(a_hat, b_hat)
    adj_bottom_MINT <- mint_betastar(W1, y_hat = y_hat)
    adj_agg_MINT    <- Sagg %*% adj_bottom_MINT
    
    adj_bottom_MINT <- as.numeric(adj_bottom_MINT)
    adj_agg_MINT    <-  as.numeric(adj_agg_MINT)
  }
  ########################
  # ADJ MEANCOMB
  #stop("done")
  adj_bottom_meancomb <- as.numeric(-mean_bottom_idtest + revisedmean_bottom_idtest)
  adj_agg_meancomb    <- as.numeric(Sagg %*% adj_bottom_meancomb)
  
  samples_agg <- array(NA, c(M, n_agg, length(agg_methods)))
  for(iagg_method in seq_along(agg_methods)){
    agg_method <- agg_methods[iagg_method]
    if(agg_method == "BASE"){
      samples_agg_method <- base_samples_agg
    }else if(agg_method == "NAIVEBU"){
      samples_agg_method <- t(tcrossprod(Sagg, apply(base_samples_bottom, 2, sample)))
    }else if(agg_method == "PERMBU"){
      samples_agg_method <- t(tcrossprod(Sagg, perm_samples_bottom))
    #}else if(agg_method == "NAIVEBU-MINT"){
    #  samples_agg_method <- t(t(samples_agg[, , match("NAIVEBU", agg_methods)]) + adj_agg_MINT)
    }else if(agg_method == "PERMBU-MINT"){
      samples_agg_method <- t(t(samples_agg[, , match("PERMBU" , agg_methods)]) + adj_agg_MINT)
    }else if(agg_method == "PERMBU-MEANCOMB"){
      samples_agg_method <- t(t(samples_agg[, , match("PERMBU" , agg_methods)]) + adj_agg_meancomb)
    }else{
      stop("error")
    }
    samples_agg[, , iagg_method] <- samples_agg_method
  }
  list_samples_agg[[idtest]] <- samples_agg
  
  # CRPS
  aggmethods_crps <- compute_crps(agg_methods, n_agg, samples_agg, obs_agg_idtest)
  list_crps_agg[[idtest]] <- aggmethods_crps
  
  # QS
  qscores_agg <- compute_qscores(agg_methods, n_agg, samples_agg, obs_agg_idtest)
  sum_overtest_qscores_agg <- sum_overtest_qscores_agg + qscores_agg
    
  ### BOTTOM
  samples_bot <- array(NA, c(M, n_bottom, length(bot_methods)))
  for(ibot_method in seq_along(bot_methods)){
    bot_method <- bot_methods[ibot_method]
    if(bot_method == "BASE"){
      samples_bot_method <- base_samples_bottom
    }else if(bot_method == "BASE-MINT"){
      samples_bot_method <- t(t(base_samples_bottom) + adj_bottom_MINT)
    }else if(bot_method == "BASE-MEANCOMB"){
      samples_bot_method <- t(t(base_samples_bottom) + adj_bottom_meancomb)
    }else{
      stop("error")
    }
    samples_bot[, , ibot_method] <- samples_bot_method
  }

  # CRPS
  botmethods_crps <- compute_crps(bot_methods, n_bottom, samples_bot, obs_bottom_idtest)
  list_crps_bot[[idtest]] <- botmethods_crps
  
  # MSE
  mse_matrix_bottom <- matrix(NA, nrow = n_bottom, ncol = length(bot_methods))
  mse_matrix_agg    <- matrix(NA, nrow = n_agg, ncol = length(agg_methods))
  
  mse_matrix_bottom[, match("BASE", bot_methods)] <- (obs_bottom_idtest - mean_bottom_idtest)^2
  revisedMINT_bottom_idtest <- mean_bottom_idtest + adj_bottom_MINT
  mse_matrix_bottom[, match("BASE-MINT", bot_methods)] <- (obs_bottom_idtest - revisedMINT_bottom_idtest)^2 
  mse_matrix_bottom[, match("BASE-MEANCOMB", bot_methods)] <- (obs_bottom_idtest - revisedmean_bottom_idtest)^2  
  list_mse_bot[[idtest]] <- mse_matrix_bottom
  
  mse_matrix_agg[, match("BASE", agg_methods)] <- (obs_agg_idtest - mean_agg_idtest)^2  
  mse_matrix_agg[, match("NAIVEBU", agg_methods)] <- (obs_agg_idtest - (Sagg %*% mean_bottom_idtest))^2  
  mse_matrix_agg[, match("PERMBU", agg_methods)] <- (obs_agg_idtest - (Sagg %*% mean_bottom_idtest))^2  
  mse_matrix_agg[, match("PERMBU-MINT", agg_methods)] <-  (obs_agg_idtest - (Sagg %*% revisedMINT_bottom_idtest))^2
  mse_matrix_agg[, match("PERMBU-MEANCOMB", agg_methods)] <- (obs_agg_idtest - (Sagg %*% revisedmean_bottom_idtest))^2 
  list_mse_agg[[idtest]] <- mse_matrix_agg
  
  # QS
  #qscores_bottom <- compute_qscores(bot_methods, n_bottom, samples_bot, obs_bottom_idtest)
  #sum_overtest_qscores_bot <- sum_overtest_qscores_bot + qscores_bottom
}# idtest

avg_qscores_agg <- sum_overtest_qscores_agg/length(allidtest)
#avg_qscores_bot <- sum_overtest_qscores_bot/length(allidtest)

res_job <- file.path(loss.folder, paste("results_HTS_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
#save(file = res_job, list = c("list_crps_agg", "list_crps_bot", "avg_qscores_agg", "avg_qscores_bot"))
save(file = res_job, list = c("list_crps_agg", "list_crps_bot", "avg_qscores_agg", "list_mse_bot", "list_mse_agg"))

samples_job <- file.path(work.folder, "samples_agg", paste("samples_agg_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
save(file = samples_job, list = c("list_samples_agg"))

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
