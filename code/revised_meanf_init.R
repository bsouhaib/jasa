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

allinputSeries <- c(aggSeries, bottomSeries)
n_inputs <- length(allinputSeries)

nvar_additional <- 4
Xhat_learn <- matrix(NA, nrow = length(learn$id), ncol = n_inputs + nvar_additional)
Xhat_test <- matrix(NA, nrow = length(test$id), ncol = n_inputs + nvar_additional)

colnames(Xhat_learn)  <- c(allinputSeries, seq(nvar_additional))
colnames(Xhat_test)      <- c(allinputSeries, seq(nvar_additional))

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
  }else if(algo == "DYNREG" || algo == "DETS"){
    insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", xvar_series, "_", algo, "_", 1, ".Rdata", sep = "")) 
  }
  
  #
  load(insample_condmean_file)
  if(algo == "KD-IC-NML"){
    all_mu <- unlist(all_mu)  
    all_mu <- c(rep(NA, n_past_obs_kd), all_mu)
  }
  Xhat_learn[, i_xvar] <-  all_mu

if(algo == "KD-IC-NML"){ 
	stop("done")
}
 
  #
  res_file <- file.path(basef.folder, algo, paste("results_", xvar_series, "_", algo, ".Rdata", sep = "")) 
  load(res_file) # all_mf
  Xhat_test[, i_xvar] <- unlist(all_mf)
}

  Xhat_learn[, n_inputs + 1] <- calendar$periodOfDay[learn$id]
  Xhat_test[, n_inputs + 1]     <- calendar$periodOfDay[test$id]
  colnames(Xhat_learn)[n_inputs + 1] <- "PDAY"
  colnames(Xhat_test)[n_inputs + 1]     <- "PDAY"
      
  Xhat_learn[, n_inputs + 2] <- calendar$tyear[learn$id]
  Xhat_test[, n_inputs + 2]     <- calendar$tyear[test$id]
  colnames(Xhat_learn)[n_inputs + 2] <- "TYEAR"
  colnames(Xhat_test)[n_inputs + 2]     <- "TYEAR"
  
  Fmat_learn <- fourier.series(calendar$tyear[learn$id], 1, 365.25)
  Fmat_test  <- fourier.series(calendar$tyear[test$id] , 1, 365.25)
  
  Xhat_learn[, n_inputs + 3] <- Fmat_learn[, 1]
  Xhat_test[, n_inputs + 3] <- Fmat_test[, 1]
  colnames(Xhat_learn)[n_inputs + 3] <- "C1"
  colnames(Xhat_test)[n_inputs + 3]     <- "C1"

  
  Xhat_learn[, n_inputs + 4] <- Fmat_learn[, 2]
  Xhat_test[, n_inputs + 4] <- Fmat_test[, 2]
  colnames(Xhat_learn)[n_inputs + 4] <- "S1"
  colnames(Xhat_test)[n_inputs + 4]     <- "S1"
  
  pday_learn <- Xhat_learn[, "PDAY"]
  pday_test  <- Xhat_test[, "PDAY"]
  
  Xhat_learn <- Xhat_learn[, -which(colnames(Xhat_learn) %in% c("PDAY", "TYEAR"))]
  Xhat_test  <- Xhat_test[, -which(colnames(Xhat_test) %in% c("PDAY", "TYEAR"))]

res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_Xmatrix_", algo.agg, "_", algo.bottom, ".Rdata", sep = "")) 
save(file = res_file, list = c("Xhat_learn", "Xhat_test", "pday_learn", "pday_test"))


#Xhat_learn[, ncol(Xhat_learn) - 1] <- as.factor(calendar$periodOfDay[learn$id])
#Xhat_test[, ncol(Xhat_test) - 1] <- as.factor(calendar$periodOfDay[test$id])
#Xhat_learn[, ncol(Xhat_learn)] <- as.factor(calendar$tyear[learn$id])
#Xhat_test[, ncol(Xhat_test)] <- as.factor(calendar$tyear[test$id])
#X_learn_fourier <- fourier.series(calendar$periodOfDay[learn$id], 2, 48)
#Xhat_test_fourier <- fourier.series(calendar$periodOfDay[test$id], 2, 48)
#nvar <- ncol(Xhat_learn)
#Xhat_learn[, seq(nvar - nvar_additional + 1, nvar)] <- X_learn_fourier
#Xhat_test[, seq(nvar - nvar_additional + 1, nvar)] <- Xhat_test_fourier
