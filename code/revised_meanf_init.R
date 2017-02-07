rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)
library(Matrix)
library(glmnet)

load(file.path(work.folder, "myinfo.Rdata"))

algo.agg <- "DYNREG"
algo.bottom <- "KD-IC-NML"

allinputSeries <- c(aggSeries, bottomSeries)
n_inputs <- length(allinputSeries)

nvar_additional <- 2
Xhat_learn <- matrix(NA, nrow = length(learn$id), ncol = n_inputs + nvar_additional)
X_test <- matrix(NA, nrow = length(test$id), ncol = n_inputs + nvar_additional)

for(i_xvar in seq_along(allinputSeries)){
  
  xvar_series <- allinputSeries[i_xvar]
  #print(xvar_series)
  
  if(xvar_series %in% bottomSeries){
    algo <- algo.bottom
  }else if(xvar_series %in% aggSeries){
    algo <- algo.agg
  }
  
  if(algo == "KD-IC-NML"){
    insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", xvar_series, "_", algo, ".Rdata", sep = "")) 
  }else if(algo == "DYNREG"){
    insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", xvar_series, "_", algo, "_", 1, ".Rdata", sep = "")) 
  }
  
  #
  load(insample_condmean_file)
  if(algo == "KD-IC-NML"){
    all_mu <- unlist(all_mu)  
    all_mu <- c(rep(NA, n_past_obs_kd), all_mu)
  }
  Xhat_learn[, i_xvar] <-  all_mu
  
  #
  res_file <- file.path(basef.folder, algo, paste("results_", xvar_series, "_", algo, ".Rdata", sep = "")) 
  load(res_file) # all_mf
  X_test[, i_xvar] <- unlist(all_mf)
}

if(nvar_additional != 0){
  Xhat_learn[, ncol(Xhat_learn)] <- as.factor(calendar$periodOfDay[learn$id])
  X_test[, ncol(X_test)] <- as.factor(calendar$periodOfDay[test$id])
  
  Xhat_learn[, ncol(Xhat_learn) - 1] <- as.factor(calendar$tyear[learn$id])
  X_test[, ncol(X_test) - 1] <- as.factor(calendar$tyear[test$id])
  
  #X_learn_fourier <- fourier.series(calendar$periodOfDay[learn$id], 2, 48)
  #X_test_fourier <- fourier.series(calendar$periodOfDay[test$id], 2, 48)
  #nvar <- ncol(Xhat_learn)
  #Xhat_learn[, seq(nvar - nvar_additional + 1, nvar)] <- X_learn_fourier
  #X_test[, seq(nvar - nvar_additional + 1, nvar)] <- X_test_fourier
}

#my_penalty <- rep(1, ncol(Xhat_learn))
#my_penalty <- c(rep(1, ncol(Xhat_learn) - nvar_additional), rep(0, nvar_additional))
my_penalty <- c(rep(1, ncol(Xhat_learn) - nvar_additional), 0, 1) # time of year can be penalized

# TRY TO KEEP THE ACTUAL MEAN UNPENALIZED
# vec <- rep(1, ncol(Xhat_learn) - nvar_additional)
# vec[match(idseries, allinputSeries)] <- 0
# my_penalty <- c(vec, rep(0, nvar_additional))

res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_Xmatrix.Rdata", sep = "")) 
save(file = res_file, list = c("Xhat_learn", "X_test", "my_penalty", "nvar_additional", "allinputSeries"))

