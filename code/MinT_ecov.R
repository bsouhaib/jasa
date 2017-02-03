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

stop(" ATTENTION. I NEED TO PRODUCE THE UNSCALED RESIDUALS FOR BOTH BOTTOM AND AGG ")

algo.bottom  <- "KD-IC-NML"
algo.agg <- "TBATS"

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
      res_file <- file.path(basef.folder, "KD-IC-NML", paste("results_", idseries, "_", "KD-IC-NML", ".Rdata", sep = "")) 
      load(res_file)
      e_residuals <- sapply(tail(learn$id, -n_past_obs_kd), function(id){
        iday <- getInfo(id)$iday
        iday <- iday - n_past_obs_kd/48
        hour <- getInfo(id)$hour
        
        if(hour %in% hours_night){
          index <- match(hour, hours_night)
          residhat <- res_residuals$res_nighthours[[iday]][[index]]$residuals
        }else{
          index <- match(hour, hours_day)
          residhat <- res_residuals$res_dayhours[[iday]][[index]]$residuals
        }
        residhat
      })
      e_residuals <- c(rep(NA, n_past_obs_kd), e_residuals)
      
    }else if(algo == "TBATS"){
      resid_file <- file.path(residuals.folder, "TBATS", paste("residuals_", idseries, "_", "TBATS", "_", 1, ".Rdata", sep = "")) 
      load(resid_file)
      #e_residuals
    }
    e_residuals
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

