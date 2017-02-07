rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
source("hts.R")

library(Matrix)
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

algo.bottom  <- "KD-IC-NML"
algo.agg <- "DYNREG"

n_bottom <- length(bottomSeries)
n_total <- n_agg + n_bottom

R_onestep <- matrix(NA, nrow = length(learn$id) - n_past_obs_kd, ncol = n_total)

for(do.agg in c(TRUE, FALSE)){
  if(do.agg){
    set_series <- aggSeries
    algo <- algo.agg
  }else{
    set_series <- bottomSeries
    algo <- algo.bottom
  }
  
  mat_residuals <- sapply(seq_along(set_series), function(j){
    if(j%%100 == 0)
    print(j)
    
    idseries <- set_series[j]
    if(algo == "KD-IC-NML"){
      resid_MINT_file <- file.path(insample.folder, algo, paste("residuals_MINT_", idseries, "_", algo, ".Rdata", sep = "")) 
      load(resid_MINT_file) # residuals_MINT
      e_vec <- c(rep(NA, n_past_obs_kd), residuals_MINT)
    }else if(algo == "DYNREG"){
      resid_MINT_file <- file.path(insample.folder, algo, paste("residuals_MINT_", idseries, "_", algo, "_", 1, ".Rdata", sep = "")) 
      load(resid_MINT_file)
      e_vec <- residuals_MINT
    }
    e_vec
  })
  print(dim(mat_residuals))
  mat_residuals <- tail(mat_residuals, -n_past_obs_kd)
  if(do.agg){
    R_onestep[, seq(n_agg)] <- mat_residuals
  }else{
    R_onestep[, seq(n_agg + 1, n_total)] <- mat_residuals
  }
}

covmethods  <- c("shrink", "blockdiagonalshrink", "diagonal", "sample")

for(covmethod in covmethods){
  W1file <- file.path(work.folder, "wmatrices", paste("W1_", algo.agg, "_", algo.bottom, "_", covmethod, ".Rdata", sep = "")) 
  if(covmethod == "diagonal"){
    W1 <- Diagonal(x = vec_w(R_onestep))
  }else if(covmethod == "sample"){
    W1 <- crossprod(R_onestep) / nrow(R_onestep)
  }else if(covmethod == "shrink"){
    target_diagonal <- lowerD(R_onestep)
    shrink_results <- shrink.estim(R_onestep, target_diagonal)
    W1 <- shrink_results$shrink.cov
  }else if(covmethod == "blockdiagonalshrink"){
    load(file.path(work.folder, "wmatrices", paste("W1_", algo.agg, "_", algo.bottom, "_", "shrink", ".Rdata", sep = "")))
    W1_bdiagonal <- W1
    W1_bdiagonal[seq(n_agg), seq(n_agg + 1, n_total)] <- 0
    W1_bdiagonal[seq(n_agg + 1, n_total), seq(n_agg)] <- 0
    W1 <- W1_bdiagonal
  }else{
    stop("error !")
  }
  save(file = W1file, list = c("W1"))
}

