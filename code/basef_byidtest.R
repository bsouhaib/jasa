rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

library(ff)
load(file.path(work.folder, "myinfo.Rdata"))


algo.agg <- "DYNREG"
algo.bottom <- "KD-IC-NML"
print("KD uses 100 quantiles while DYNREG uses 21 quantiles")
ntest <- length(test$id)

QF_bottom <- vector("list", length(bottomSeries))
QF_agg <- vector("list", length(aggSeries))

obs_agg    <- matrix(NA, nrow =  length(aggSeries), ncol = ntest)
obs_bottom <- matrix(NA, nrow =  length(bottomSeries), ncol = ntest)

for(do.agg in c(TRUE, FALSE)){
  
  if(do.agg){
    set_series <- aggSeries
    algo <- algo.agg
  }else{
    set_series <- bottomSeries
    algo <- algo.bottom
  }
  
  for(j in seq_along(set_series)){
   # if(j%%100 == 0)
    print(j)
    
    if(do.agg){
      QF_agg[[j]] <- matrix(NA, nrow = length(taus), ncol = ntest)
    }else{
      QF_bottom[[j]] <- matrix(NA, nrow = length(taus), ncol = ntest)
    }
    
    idseries <- set_series[j]
    
    res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
    load(res_file)
    
    if(do.agg){
      load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
      obs_agg[j, ] <- demand[test$id]
    }else{
      load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
      obs_bottom[j, ] <- demand[test$id]
    }
    
    for(idtest in seq(ntest)){
      iday <- getInfo(idtest)$iday
      hour <- getInfo(idtest)$hour
      
      if(do.agg){
        QF_agg[[j]][, idtest] <- all_qf[[iday]][, hour]
      }else{
        QF_bottom[[j]][, idtest] <- all_qf[[iday]][, hour]
      }
    }# idtest
  }# series
}# AGG and BOTTOM

#stop("done")
dir.create(file.path(basef.folder, "byidtest"), recursive = TRUE, showWarnings = FALSE)

for(idtest in seq(ntest)){
  print(idtest)
  res_byidtest_file <- file.path(basef.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = ""))
  
  QF_agg_idtest <- sapply(seq(length(aggSeries)), function(j){
    QF_agg[[j]][, idtest]
  })
  
  QF_bottom_idtest <- sapply(seq(length(bottomSeries)), function(j){
    QF_bottom[[j]][, idtest]
  })
  
  obs_agg_idtest <- obs_agg[, idtest]
  obs_bottom_idtest <- obs_bottom[, idtest]

  save(file = res_byidtest_file, list = c("QF_agg_idtest", "QF_bottom_idtest", 
                                          "obs_agg_idtest", "obs_bottom_idtest"))    
}
