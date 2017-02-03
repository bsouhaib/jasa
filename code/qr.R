rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)
library(Matrix)

library(quantreg)
library(rqPen)

load(file.path(work.folder, "myinfo.Rdata"))

nbpast <- 48 * 7 * 4 * 3 + length(validation$id)
idt <- tail(learn$id, nbpast)

algo.agg <- "DYNREG"
algo.bottom <- "KD-IC-NML"

idseries <- bottomSeries[230]
node <- V(itree)[match(idseries, names(V(itree)))]
ancestors <- subcomponent(itree, node, mode = "in")[-1]
siblings_nodes <- ego(itree, order = 1, nodes = ancestors[1], mode = "out")[[1]][-1]
agg_nodes_needed <- names(ancestors)
bottom_nodes_needed <- names(siblings_nodes)
#match(bottom_nodes_needed, bottomSeries)
#match(agg_nodes_needed, aggSeries)
regressors_nodes <- c(agg_nodes_needed, bottom_nodes_needed)

load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
demand_idseries <- demand
  
  X_hat <- matrix(NA, nrow = nbpast, ncol = length(regressors_nodes) * 20)
  for(ireg in seq_along(regressors_nodes)){
    idseries_reg <- regressors_nodes[ireg]
    print(idseries_reg)
    
    if(idseries_reg %in% bottomSeries){
      algo <- algo.bottom
    }else if(idseries_reg %in% aggSeries){
      algo <- algo.agg
    }
    
    if(algo == "KD-IC-NML"){
      insample_quantile_file <- file.path(insample.folder, algo, 
                                          paste("quantiles_", idseries_reg, "_", algo, ".Rdata", sep = "")) 
    }else if(algo == "DYNREG"){
      insample_quantile_file <- file.path(residuals.folder, "DYNREG", 
                                          paste("quantiles_", idseries_reg, "_", algo, "_", 1, ".Rdata", sep = ""))
    }
      
    #alldays <- getInfo(tail(learn$id, -n_past_obs_kd))$iday - n_past_obs_kd/48
      
    res <- lapply(seq_along(all_qfe), function(iday){
        v <- sapply(seq(48), function(hour){
          
          myalphas <- alphas
          if(algo == "KD-IC-NML"){
            myalphas <- all_tau_insample[[iday]][, hour]
          }
          invcdf <- approxfun(myalphas, all_qfe_insample[[iday]][, hour], rule = 2)
          qf <- invcdf(seq(0.01, 0.99, 0.05))
          qf
        })
        t(v)
      })  
    mat <- do.call(rbind, res)
    if(algo == "KD-IC-NML"){
        mat_na <- matrix(rep(rep(NA, 20), n_past_obs_kd), ncol = 20, byrow = T)
        mat <- rbind(mat_na, mat)
    }
    mat <- tail(mat, nbpast)
    
    X_hat[, (ireg - 1) * 20 + seq(20)] <- mat
    
    if(FALSE){
      matplot(t(mat), lty = 1, type = 'l', col = "black")
      z <- as.numeric(y)
      lines(z, col = "red")
    }
    
  }# reg

  y <- demand_idseries[idt]
  stop("done")
  
  # Split train and validation + h-1, h and h+1
  
  plot.ts(y[seq(48*7)])
  lines(X_hat[seq(48 * 7), 19], col = "red")
  
  # CAREFUL WITH THE NONSTATIONARIY OF THE INPUT VARIABLES
  f1 <- rq(y ~ X_hat, method="lasso",lambda = 30, tau = 0.11)
  plot.ts(X_hat[, 3])
  lines(fitted(f1), col = "red")
  
  f1 <- rq(y ~ X_hat, method="lasso",lambda = 30, tau = 0.1)
  f2 <- rq(y ~ X_hat, method="lasso",lambda = 30, tau = 0.5)
  f3 <- rq(y ~ X_hat, method="lasso",lambda = 30, tau = 0.9)
  
  lassoModel <- rq.lasso.fit.mult(X_hat,y,lambda=1)
  
  groupMultLambda(x,y,groups=c(rep(1,2),rep(2,2)), lambda = seq(.1,.5,.1))
  groupMultLambda(X_hat, y, groups, tau = 0.5, lambda, intercept = TRUE, penalty = "LASSO", alg="QICD")
  
  median_hat <- fitted(f)
