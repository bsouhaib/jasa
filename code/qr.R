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

algo.agg <- "TBATS"
algo.bottom <- "KD-IC-NML"

myseries <- bottomSeries[100]
for(idseries in myseries){
  
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  demand_idseries <- demand
  
  #series_regressors <- c(bottomSeries[1], aggSeries)
  series_regressors <- c(bottomSeries[100], head(aggSeries, 5))
  
  X_hat <- matrix(NA, nrow = nbpast, ncol = length(series_regressors) * 20)
  for(ireg in seq_along(series_regressors)){
    idseries_reg <- series_regressors[ireg]
    print(idseries_reg)
    
    if(idseries_reg %in% bottomSeries){
      algo <- algo.bottom
    }else if(idseries_reg %in% aggSeries){
      aogo <- algo.agg
    }
    
    if(algo == "KD-IC-NML"){
      insamplecdf_file <- file.path(insample.folder, algo, paste("insamplecdf_", idseries, "_", algo, ".Rdata", sep = "")) 
      load(insamplecdf_file)
      
      #alldays <- getInfo(tail(learn$id, -n_past_obs_kd))$iday - n_past_obs_kd/48
      
      res <- lapply(seq_along(all_qf), function(iday){
        v <- sapply(seq(48), function(hour){
          invcdf <- approxfun(all_tau[[iday]][, hour], all_qf[[iday]][, hour], rule = 2)
          qf <- invcdf(seq(0.01, 0.99, 0.05))
          qf
        })
        t(v)
      })  
      mat <- do.call(rbind, res)
      mat_na <- matrix(rep(rep(NA, 20), n_past_obs_kd), ncol = 20, byrow = T)
      mat <- rbind(mat_na, mat)
      mat <- tail(mat, nbpast)
      
    }else if(algo == "TBATS"){
      resid_file <- file.path(residuals.folder, "TBATS", paste("residuals_", idseries_reg, "_", algo, "_", 1, ".Rdata", sep = "")) 
      load(resid_file)
     
      e_tbats <- tail(e_residuals, nbpast)
      my_var <- mean(e_tbats^2)
      
      load(file.path(aggseries.folder, paste("series-", idseries_reg, ".Rdata", sep = "")))
      demand_ireg <- demand
      y <- demand_ireg[idt]
      mu_hat <- y - e_tbats
      
      mat <- sapply(mu_hat, function(mu){
        qnorm(seq(0.01, 0.99, 0.05), mean = mu, sd = sqrt(my_var))
      })
      
     
    }
    
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
  
}

# are the quantile forecasts for in-sample available for both KD AND TBATS????

