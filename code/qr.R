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

algo <- "TBATS"

for(idseries in bottomSeries){
  
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  demand_idseries <- demand
  
  series_regressors <- aggSeries
  X_hat <- matrix(NA, nrow = nbpast, ncol = length(series_regressors) * 20)
  for(ireg in seq_along(series_regressors)){
    
    idseries_reg <- series_regressors[ireg]
    print(idseries_reg)
    if(algo == "KD-IC-NML"){
      #
    }else if(algo == "TBATS"){
      resid_file <- file.path(residuals.folder, "TBATS", paste("residuals_", idseries_reg, "_", "TBATS", "_", 1, ".Rdata", sep = "")) 
      load(resid_file)
      load(file.path(aggseries.folder, paste("series-", idseries_reg, ".Rdata", sep = "")))
      demand_ireg <- demand
      
      y <- demand_ireg[idt]
      e_tbats <- tail(e_residuals, nbpast)
      my_var <- mean(e_tbats^2)
      mu_hat <- y - e_tbats
      
      mat <- sapply(mu_hat, function(mu){
        qnorm(seq(0.01, 0.99, 0.05), mean = mu, sd = sqrt(my_var))
      })
      
      X_hat[, (ireg - 1) * 20 + seq(20)] <- mat
      
      if(FALSE){
        matplot(t(mat), lty = 1, type = 'l', col = "black")
        z <- as.numeric(y)
        lines(z, col = "red")
      }
    }
  }# reg
  
  
  y <- demand_idseries[idt]
  stop("done")
  
  # Split train and validation + h-1, h and h+1
  
  # CAREFUL WITH THE NONSTATIONARIY OF THE INPUT VARIABLES
  
  f1 <- rq(y ~ X_hat, method="lasso",lambda = 30, tau = 0.1)
  f2 <- rq(y ~ X_hat, method="lasso",lambda = 30, tau = 0.5)
  f3 <- rq(y ~ X_hat, method="lasso",lambda = 30, tau = 0.9)
  
  lassoModel <- rq.lasso.fit.mult(X_hat,y,lambda=1)
  
  groupMultLambda(x,y,groups=c(rep(1,2),rep(2,2)), lambda = seq(.1,.5,.1))
  groupMultLambda(X_hat, y, groups, tau = 0.5, lambda, intercept = TRUE, penalty = "LASSO", alg="QICD")
  
  median_hat <- fitted(f)
  
}

# are the quantile forecasts for in-sample available for both KD AND TBATS????

