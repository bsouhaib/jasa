rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

alliseries <- seq(length(bottomSeries))
algo <- "KD-IC-NML"

ntest <- length(test$id)
allidtest <- seq(ntest)

list_invcdf <- vector("list", length(alliseries))
#for(j in seq_along(alliseries)){
#  list_invcdf[[j]] <- vector("list", ntest)
#}
  
for(j in seq_along(alliseries)){
  print(j)
  iseries <- alliseries[j]
  idseries <- bottomSeries[iseries]
  
  res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
  load(res_file)
  
  list_invcdf[[j]] <- lapply(allidtest, function(idtest){
    
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
      invcdf <- approxfun(tauhat, qtauhat, rule = 2)
    }else{
      stop("error")
    }
    
    invcdf
    
  })
  
}
  