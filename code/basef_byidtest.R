rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

library(ff)
load(file.path(work.folder, "myinfo.Rdata"))


algo.agg <- "TBATS"
algo.bottom <- "KD-IC-NML"
print("KD uses 100 quantiles while TBATS uses 21 quantiles")
ntest <- length(test$id)

#QF_bottom <- PROB_bottom <- array(NA, c(100, ntest, length(bottomSeries)))
#QF_bottom <- PROB_bottom <- ff(NA, dim = c(100, ntest, length(bottomSeries)))
QF_bottom <- PROB_bottom <- vector("list", length(bottomSeries))

if(algo.agg == "KD-IC-NML"){
  #  QF_agg <- PROB_agg  <- array(NA, c(100, ntest, length(aggSeries)))
  # QF_agg <- PROB_agg  <- ff(NA, dim = c(100, ntest, length(aggSeries)))
  QF_agg <- PROB_agg  <- vector("list", length(aggSeries))
}else{
  # QF_agg <- array(NA, c(length(alphas), ntest, length(aggSeries)))
  # QF_agg <- ff(NA, dim = c(length(alphas), ntest, length(aggSeries)))
  QF_agg <- vector("list", length(aggSeries))
}

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
      if(algo.agg == "KD-IC-NML"){
        QF_agg[[j]] <- PROB_agg[[j]] <- matrix(NA, nrow = 100, ncol = ntest)
      }else if(algo.agg == "TBATS"){
        QF_agg[[j]] <- matrix(NA, nrow = length(alphas), ncol = ntest)
      }
    }else{
      QF_bottom[[j]] <- PROB_bottom[[j]] <- matrix(NA, nrow = 100, ncol = ntest)
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
      
      if(algo == "Uncond" || algo == "PeriodOfDay"){
        invcdf <- approxfun(alphas, qFtest[, idtest], rule = 2)
      }else if(algo == "TBATS"){	
        invcdf <- approxfun(alphas, all_qf[[iday]][, hour], rule = 2)
      }else if(algo == "KD-IC-NML"){	
        if(hour %in% hours_night){
          index <- match(hour, hours_night)
          qtauhat <- res_testing$res_nighthours[[iday]][[index]]$qtauhat
          tauhat <- res_testing$res_nighthours[[iday]][[index]]$tauhat
        }else{
          index <- match(hour, hours_day)
          qtauhat <- res_testing$res_dayhours[[iday]][[index]]$qtauhat
          tauhat <- res_testing$res_dayhours[[iday]][[index]]$tauhat
        }
        #invcdf <- approxfun(tauhat, qtauhat, rule = 2)
      }else{
        stop("error")
      }
      
      if(do.agg){
        if(algo.agg == "KD-IC-NML"){
          #QF_agg[, idtest, j] <- qtauhat
          #PROB_agg[, idtest, j] <- tauhat
          QF_agg[[j]][, idtest] <- qtauhat
          PROB_agg[[j]][, idtest] <- tauhat
        }else if(algo.agg == "TBATS"){
          #QF_agg[, idtest, j] <- all_qf[[iday]][, hour]
          QF_agg[[j]][, idtest] <- all_qf[[iday]][, hour]
        }
      }else{
        if(algo.bottom == "KD-IC-NML"){
          #QF_bottom[, idtest, j] <- qtauhat
          #PROB_bottom[, idtest, j] <- tauhat
          QF_bottom[[j]][, idtest] <- qtauhat
          PROB_bottom[[j]][, idtest] <- tauhat
        }
      }
    }# idtest
  }# series
}# AGG and BOTTOM

#stop("done")

for(idtest in seq(ntest)){
  print(idtest)
  res_byidtest_file <- file.path(basef.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = ""))
  
  #QF_agg <- QF_agg[, idtest,]
  #QF_bottom <- QF_bottom[, idtest,]
  #PROB_bottom <- PROB_bottom[, idtest,]
  
  QF_agg_idtest <- sapply(seq(length(aggSeries)), function(j){
    QF_agg[[j]][, idtest]
  })
  
  QF_bottom_idtest <- sapply(seq(length(bottomSeries)), function(j){
    QF_bottom[[j]][, idtest]
  })
  
  PROB_bottom_idtest <- sapply(seq(length(bottomSeries)), function(j){
    PROB_bottom[[j]][, idtest]
  })
  
  obs_agg_idtest <- obs_agg[, idtest]
  obs_bottom_idtest <- obs_bottom[, idtest]
    
  if(algo.agg == "TBATS" && algo.bottom == "KD-IC-NML"){
    save(file = res_byidtest_file, list = c("QF_agg_idtest", "QF_bottom_idtest", "PROB_bottom_idtest", 
                                            "obs_agg_idtest", "obs_bottom_idtest"))
  }else if(algo.agg == "KD-IC-NML" && algo.bottom == "KD-IC-NML"){
    #PROB_agg <- PROB_agg[, idtest,]
    PROB_agg_idtest <- sapply(seq(length(aggSeries)), function(j){
      PROB_agg[[j]][, idtest]
    })
    save(file = res_byidtest_file, list = c("QF_agg_idtest", "PROB_agg_idtest", "QF_bottom_idtest", "PROB_bottom_idtest", 
                                            "obs_agg_idtest", "obs_bottom_idtest"))
  }else{
    stop("error !")
  }
}
