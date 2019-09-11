rm(list = ls())
args = (commandArgs(TRUE))
if(length(args) == 0){
  idjob <- 1
  allidtest <- seq(35, 4416, by = 48) #1:4 #1:123 #1:4 #1:1104 
}else{
  
  #for(i in 1:length(args)){
  #  print(args[[i]])
  #}
  
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
library(quadprog)

set.seed(1986)

MAT <- MAT2 <- NULL

compute_crps <- function(methods, n, mat_samples, observations){
  res <- sapply(seq_along(methods), function(imethod){
    sapply(seq(n), function(i){
      crps_sampling(mat_samples[, i, imethod], observations[i])
    })
  })
  colnames(res) <- methods
  res
}

OLD_compute_qscores <- function(methods, n, mat_samples, observations){
  print(base::date())
  sorted_samples <- apply(mat_samples, c(2, 3), sort)
  print(base::date())
  qscores <- sapply(seq(n), function(i){
    obs <- observations[i]
    sapply(seq_along(methods), function(imethod){
      sapply(seq(length(q_probs)), function(iprob){
        qf <- sorted_samples[iprob, i, imethod]
        2 * ((obs <= qf) - q_probs[iprob]) * (qf - obs)
      })
    })
  }, simplify = 'array')
  print(base::date())
  qscores
}

weighted_crps <- function(qscores, weights){
  sum(qscores * weights)/length(qscores)
}

compute_qscores <- function(methods, n, mat_samples, observations){
  sorted_samples <- apply(mat_samples, c(2, 3), sort)
  qscores <- sapply(seq(n), function(i){
    obs <- observations[i]
    sapply(seq_along(methods), function(imethod){
      qf <- sorted_samples[, i, imethod]
      2 * ((obs <= qf) - q_probs) * (qf - obs)      
    })
  }, simplify = 'array')
  qscores
}

#if(idjob == 34){
#  allidtest <- 4291:4416
#}

print(allidtest)


load(file.path(work.folder, "myinfo.Rdata"))
n_bottom <- length(bottomSeries)

bot_methods <- c("BASE", "BASE-MINT", "BASE-MCOMB", "BASE-MCOMBRECON", "PROBMINT")
agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", 
                 "PERMBU-MCOMBRECON", "PERMBU-MCOMBUNRECON", "NAIVEBU-MINT", "PROBMINT")

#bot_methods <- c("BASE", "BASE-MINT")
#agg_methods <- c("BASE", "NAIVEBU", "PERMBU")

do.mint <- any(c(grepl("MINT", bot_methods), grepl("MINT", agg_methods)))
do.myrevmean <- any(c(grepl("MCOMB", bot_methods), grepl("MCOMB", agg_methods)))

if(do.mint){
  covmethod <- c("shrink")
  #W1file <- file.path(work.folder, "wmatrices", paste("W1_", algo.agg, "_", algo.bottom, "_", covmethod, ".Rdata", sep = "")) 
  
  #load(W1file)
  J <- Matrix(cbind(matrix(0, nrow = n_bottom, ncol = n_agg), diag(n_bottom)), sparse = TRUE)
  U <- Matrix(rbind(diag(n_agg), -t(Sagg)), sparse = TRUE)
}

P_bu <- cbind(matrix(0, nrow = n_bottom, ncol = n_agg), diag(n_bottom))
n_total <- n_agg + n_bottom
weights_GTOP <- rep(1, n_agg + n_bottom)
Rmat <- diag(sqrt(weights_GTOP))

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

list_crps_agg <- list_wcrps_agg <- list_qscores_agg <- list_mse_agg <- vector("list", ntest)
list_crps_bot <- list_wcrps_bot <- list_mse_bot  <- vector("list", ntest)
  
list_samples_agg <- vector("list", ntest)
sum_overtest_qscores_agg <- sum_overtest_qscores_bot <- 0

# LOADING PERMUTATION FILE
perm_file <- file.path(permutations.folder, paste("perm_", algo.agg, "_", algo.bottom, ".Rdata", sep = "")) 
load(perm_file) # "list_matpermutations" "list_vecties"

# Generate samples
for(idtest in allidtest){
  print(idtest)
  #print(base::date())
  #if(idtest%%48 == 0)
  #{ 
  #  print(idtest)
  #  print(base::date())
  #}
  
  res_byidtest_file <- file.path(work.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
  load(res_byidtest_file)
  # "QF_agg_idtest", "QF_bottom_idtest", "obs_agg_idtest", "obs_bottom_idtest", 
  # "revisedmean_bottom_idtest", 'revisedmean_agg_idtest', "mean_agg_idtest", "mean_bottom_idtest"
  
  iday <- getInfo(idtest)$iday
  hour <- getInfo(idtest)$hour
 
  ###### MINT ############
  #  adjustments 
  if(do.mint){
    #print("MINT")
    
    #b_hat <- apply(base_samples_bottom, 2, mean)
    #a_hat <- apply(base_samples_agg, 2, mean)
    b_hat <- mean_bottom_idtest
    a_hat <- mean_agg_idtest 
    
    hwanted <- idtest%%48
    if(hwanted == 0)
      hwanted <- 48
      
    Whfile <- file.path(work.folder, "wmatrices", paste("W_", hwanted, "_", algo.agg, "_", algo.bottom, "_", "shrink", ".Rdata", sep = ""))
    load(Whfile)
    
    y_hat <- c(a_hat, b_hat)
    adj_bottom_MINT <- mint_betastar(W1, y_hat = y_hat)
    adj_agg_MINT    <- Sagg %*% adj_bottom_MINT
    
    adj_bottom_MINT <- as.numeric(adj_bottom_MINT)
    adj_agg_MINT    <-  as.numeric(adj_agg_MINT)
    
    revisedMINT_bottom_idtest <- mean_bottom_idtest + adj_bottom_MINT
    #print("MINT done")
    #print(base::date())
    
    # MINT Variance
    P_mint <- mint_pmatrix(W1)
    S <- rbind(Sagg, diag(n_bottom))
    V_mint <- S %*% P_mint %*% W1 %*% t(P_mint) %*% t(S)
    
    Vmint_agg <- diag(V_mint)[seq(n_agg)]
    Vmint_bot <- diag(V_mint)[seq(n_agg + 1, n_total)]
      
    # a_tilde_test <- Sagg %*% P_mint %*% y_hat
    
    # stop("done")
    # library(MASS)
    # n_bottom
    # Xstandard <- mvrnorm(n = M, rep(0, n_bottom), diag(n_bottom))
    # s <- svd(Sigma)
    # b_tilde <- as.numeric(P_mint %*% t(t(y_hat)))
    # V_bottom <- V_mint[seq(n_agg + 1, n_total), seq(n_agg + 1, n_total)]
    # X <- mvrnorm(n = M, b_tilde, V_bottom)
    # Y <- t(Sagg %*% t(X))
    
   }
  ########################
  # ADJ MEANCOMB
  #stop("done")
  
  if(do.myrevmean){
    #print("GTOP")
    
    new.gtop <- TRUE
    if(new.gtop){
      y_check <- c(revisedmean_agg_idtest, revisedmean_bottom_idtest)
      rev_and_reconcilied_mean_idtest <- gtop(y_check, Rmat, n_agg, n_total, Sagg, P_bu)
      rev_and_reconcilied_bottom_mean_idtest <- P_bu %*% rev_and_reconcilied_mean_idtest
      
      # adj_bottom_meancomb <- as.numeric(-mean_bottom_idtest + rev_and_reconcilied_bottom_mean_idtest)
      # adj_bottom_meancomb <- as.numeric(-mean_bottom_idtest + revisedmean_bottom_idtest) + as.numeric(- revisedmean_bottom_idtest + rev_and_reconcilied_bottom_mean_idtest)
      # adj_agg_meancomb    <- as.numeric(Sagg %*% adj_bottom_meancomb)
      #revisedmean_bottom_idtest <- rev_and_reconcilied_bottom_mean_idtest
      
      adj_bottom_mcomb <- as.numeric(-mean_bottom_idtest + revisedmean_bottom_idtest)
      adj_agg_mcomb    <- as.numeric(Sagg %*% adj_bottom_mcomb)
        
      adj_bottom_mcombrecon <- as.numeric(-mean_bottom_idtest + rev_and_reconcilied_bottom_mean_idtest)
      adj_agg_mcombrecon    <- as.numeric(Sagg %*% adj_bottom_mcombrecon)

      # unreconcilied
      # WRONG !! #adj_agg_mcombunrecon <-  as.numeric(-mean_agg_idtest + revisedmean_agg_idtest)
      adj_agg_mcombunrecon <-  as.numeric(-Sagg %*% mean_bottom_idtest + revisedmean_agg_idtest)
      
     }else{
      # OLD MEANCOMB
      adj_bottom_meancomb <- as.numeric(-mean_bottom_idtest + revisedmean_bottom_idtest)
      adj_agg_meancomb    <- as.numeric(Sagg %*% adj_bottom_meancomb)
     }
    #print("MCOMB done")
    #print(base::date())
  }
  
  #stop("done")
  
  #print(base::date())
  
  ######################## BOTTOM
  samples_bot <- array(NA, c(M, n_bottom, length(bot_methods)))
  meanf_bot   <- matrix(NA, nrow = n_bottom, ncol = length(bot_methods))
  for(ibot_method in seq_along(bot_methods)){
    bot_method <- bot_methods[ibot_method]
    if(bot_method == "BASE"){
      samples_bot_method <- base_samples_bottom
      meanf_bot_method <- mean_bottom_idtest
    }else if(bot_method == "BASE-MINT"){
      samples_bot_method <- t(t(base_samples_bottom) + adj_bottom_MINT)
      meanf_bot_method <- revisedMINT_bottom_idtest
    }else if(bot_method == "BASE-MCOMB"){
      samples_bot_method <- t(t(base_samples_bottom) + adj_bottom_mcomb)
      meanf_bot_method <- revisedmean_bottom_idtest
    }else if(bot_method == "BASE-MCOMBRECON"){
      samples_bot_method <- t(t(base_samples_bottom) + adj_bottom_mcombrecon)
      meanf_bot_method <- rev_and_reconcilied_bottom_mean_idtest
    }else if(bot_method == "PROBMINT"){
      meanf_bot_method <- revisedMINT_bottom_idtest
      id_negative <- which(meanf_bot_method < 0)
      meanf_bot_method[id_negative] <- mean_bottom_idtest[id_negative]
      varf_bot_method  <- Vmint_bot
      
      samples_bot_method <- sapply(seq(n_bottom), function(ibot){
        m <- meanf_bot_method[ibot]
        v <- varf_bot_method[ibot]
        
        mulog <- log(m/sqrt(1+(v/m^2)))
        sdlog <- sqrt(log(1+(v/m^2)))
        
        qlnorm(q_probs, mulog, sdlog)
      })
      
    }else{
      stop("error")
    }
    samples_bot[, , ibot_method] <- samples_bot_method
    meanf_bot[, ibot_method] <- meanf_bot_method
  }

  # MSE
  mse_matrix_bottom <- (meanf_bot - obs_bottom_idtest)^2
  list_mse_bot[[idtest]] <- mse_matrix_bottom
  
  # CRPS
  botmethods_crps <- compute_crps(bot_methods, n_bottom, samples_bot, obs_bottom_idtest)
  list_crps_bot[[idtest]] <- botmethods_crps
  
  # QS
  qscores_bottom <- compute_qscores(bot_methods, n_bottom, samples_bot, obs_bottom_idtest)
  #sum_overtest_qscores_bot <- sum_overtest_qscores_bot + qscores_bottom
  
  ######################## AGG
  samples_agg <- array(NA, c(M, n_agg, length(agg_methods)))
  meanf_agg   <- matrix(NA, nrow = n_agg, ncol = length(agg_methods))
  
  for(iagg_method in seq_along(agg_methods)){
    agg_method <- agg_methods[iagg_method]
    if(agg_method == "BASE"){
      samples_agg_method <- base_samples_agg
      meanf_agg_method <- mean_agg_idtest
    }else if(agg_method == "NAIVEBU"){
      samples_agg_method <- t(tcrossprod(Sagg, apply(base_samples_bottom, 2, sample)))
      meanf_agg_method <- (Sagg %*% mean_bottom_idtest)
    }else if(agg_method == "PERMBU"){
      samples_agg_method <- t(tcrossprod(Sagg, perm_samples_bottom))
      meanf_agg_method <- (Sagg %*% mean_bottom_idtest)
    #}else if(agg_method == "NAIVEBU-MINT"){
    #  samples_agg_method <- t(t(samples_agg[, , match("NAIVEBU", agg_methods)]) + adj_agg_MINT)
    }else if(agg_method == "PERMBU-MINT"){
      samples_agg_method <- t(t(samples_agg[, , match("PERMBU" , agg_methods)]) + adj_agg_MINT)
      meanf_agg_method <- (Sagg %*% revisedMINT_bottom_idtest)
    }else if(agg_method == "NAIVEBU-MINT"){
      samples_agg_method <- t(t(samples_agg[, , match("NAIVEBU" , agg_methods)]) + adj_agg_MINT)
      meanf_agg_method <- (Sagg %*% revisedMINT_bottom_idtest)
    }else if(agg_method == "PERMBU-MCOMB"){
      samples_agg_method <- t(t(samples_agg[, , match("PERMBU" , agg_methods)]) + adj_agg_mcomb)
      meanf_agg_method <- (Sagg %*% revisedmean_bottom_idtest)
    }else if(agg_method == "PERMBU-MCOMBRECON"){
      samples_agg_method <- t(t(samples_agg[, , match("PERMBU" , agg_methods)]) + adj_agg_mcombrecon)
      meanf_agg_method <- (Sagg %*% rev_and_reconcilied_bottom_mean_idtest)
    }else if(agg_method == "PERMBU-MCOMBUNRECON"){
      samples_agg_method <- t(t(samples_agg[, , match("PERMBU" , agg_methods)]) + adj_agg_mcombunrecon)
      meanf_agg_method <- revisedmean_agg_idtest
    }else if(agg_method == "PROBMINT"){
      
      meanf_agg_method <- (Sagg %*% revisedMINT_bottom_idtest)
      varf_agg_method  <- Vmint_agg
      sd_agg_method    <- sqrt(varf_agg_method)
      
      samples_agg_method <- sapply(seq(n_agg), function(iagg){
        qnorm(q_probs, meanf_agg_method[iagg], sd_agg_method[iagg])
      })
      
    }else{
      stop("error")
    }
    samples_agg[, , iagg_method] <- samples_agg_method
    meanf_agg[, iagg_method] <- meanf_agg_method
  }

  #if(idjob %in% c(1, 2, 3, 4, 5)){
  #  list_samples_agg[[idtest]] <- samples_agg
  #}
  
 
   iagg <- 1
   #library(lattice); library(latticeExtra)
   dat <- data.frame(samples_agg[, iagg, c(1, 4, 9)]); 
   #colnames(dat) <- c("a", "b", "c"); 
   #ecdfplot(~ a+b+c, data=dat, col = c("black", "purple", "green") )
  
   #plot(ecdf(dat[, 2]), col = c("purple"))
   #lines(ecdf(dat[, 1]), col = c("black"))
   #lines(ecdf(dat[, 3]), col = c("green"))
   #abline(v = obs_agg_idtest[iagg])
   
   
   obs <- obs_agg_idtest[iagg]
   res <- sapply(seq(3), function(i){crps_sampling(dat[, i], obs)})
   print(res)
   
   if(FALSE){
   sorted_dat <- apply(dat, 2, sort)
   
   res_all <- sapply(seq(3), function(i){
     sapply(seq(length(q_probs)), function(iprob){
       qf <- sorted_dat[iprob, i]
       2 * ((obs <= qf) - q_probs[iprob]) * (qf - obs)
      })
    })
   MAT2 <- rbind(MAT2, apply(res_all, 2, mean))
   
   }
   
   MAT <- rbind(MAT, res)
   
 
}# idtest
