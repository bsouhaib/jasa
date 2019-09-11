rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

plot.permsamples <- FALSE

idays <- seq(1, 92, by = 1)
algorithms <- c("INDEPBU", "PERMBU", "MINTshrink", "INDEPBU-MINTshrink", "PERMBU-MINTshrink", "BASE")
agg_methods <- c("BASE", "INDEPBU", "PERMBU", "PERMBU-MINTshrink", "INDEPBU-MINTshrink", "MINTdiag", "MINTshrink")


for(do.agg in c(TRUE, FALSE)){
  
  
  if(do.agg){
    algorithms <- "DETS"
    alliseries <- seq(length(aggSeries))
  }else{
    algorithms <- "KD-IC-NML"
    alliseries <- seq(length(bottomSeries))
  }

  list_results <- vector("list", length(alliseries))
  
  for(iseries in alliseries){
      
    print(iseries)
    if(do.agg){
      idseries <- aggSeries[iseries]
      load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
    }else{
      idseries <- bottomSeries[iseries]
      load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
    }
  
    list_load <- vector("list", length(algorithms))
    for(ialgo in seq_along(algorithms)){
      algo <- algorithms[ialgo]
      algo_load <- algo
      
      if(plot.permsamples){
        qf <- lapply(idays, function(iday){
          all_qf[[iday]][, ialgo, ]
        })
        mf <- lapply(idays, function(iday){
          all_mf[[iday]][ialgo, ]
        })
        
        list_load[[ialgo]] <-  list(all_qf = qf, all_mf = mf)
      }else{
        res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
        load(res_file)
        
        if(algo_load == "KD-IC-NML"){
          list_load[[ialgo]] <- list(all_qf = all_qf, all_mf = all_mf) #res_testing
        }else if(algo_load == "TBATS" || algo_load == "DYNREG" || algo_load == "DETS"){
          list_load[[ialgo]] <- list(all_qf = all_qf, all_mf = all_mf)
          #list_load[[ialgo]] <- list(all_qf = all_qf, all_mf = all_mf, all_mfsample = all_mfsample)
        }else if(algo_load == "Uncond"){
          list_load[[ialgo]] <- list(qFtest = qFtest, mFtest = mFtest)
        }
      }
    }#algo
    
    list_results[[iseries]] <- vector("list", 92)
    for(iday in idays){
      day_min <- Inf
      day_max <- -Inf
      for(ialgo in seq_along(algorithms)){
        day_min <- pmin(day_min, min(list_load[[ialgo]]$all_qf[[iday]]))
        day_max <- pmax(day_max, max(list_load[[ialgo]]$all_qf[[iday]]))
      }
      
      #print(iday)
      
      for(ialgo in seq_along(algorithms)){
        
        algo <- algorithms[ialgo]
        algo_load <- algo
        
        if(algo_load == "KD-IC-NML" || algo_load == "TBATS" || algo_load == "DYNREG" || algo_load == "DETS" || plot.permsamples){
          
          all_qf <- list_load[[ialgo]]$all_qf
          all_mf <- list_load[[ialgo]]$all_mf
          mu_hat <- matrix(unlist(all_mf), ncol = 48, byrow = T)
          
          #qf_allhours <- sapply(seq(48), function(hour){
          #  qf <- all_qf[[iday]][, hour]
          #})
          qf_allhours <- all_qf[[iday]]
          
          #if(algo == "DETS")
          #browser()
          # all_mfsample <- list_load[[ialgo]]$all_mfsample
          # mu_hatsample <- matrix(unlist(all_mfsample), ncol = 48, byrow = T)
          
        }else if(algo_load == "Uncond"){
          qFtest <- list_load[[ialgo]]$qFtest
          mFtest <- list_load[[ialgo]]$mFtest
          
          qf_allhours <- qFtest
          mu_hat <- matrix(mFtest, ncol = 48, byrow = T)
        }
        
        rownames(qf_allhours) <- paste(taus*100, "%", sep = "")
        
        future <- demand[test$id[(iday - 1) * 48 + seq(1, 48)]]
        subtaus    <- c("5%", "25%", "75%", "95%")
        
        # COVERAGE
        
        # qf_allhours["75%", ]
        # qf_allhours["25%", ]
        # future > qf_allhours["25%", ] & future < qf_allhours["75%", ]
        
        cov50 <- 100 * length(which(future >= qf_allhours["25%", ] & future <= qf_allhours["75%", ]))/48
        cov90 <- 100 * length(which(future >= qf_allhours["5%", ] & future <= qf_allhours["95%", ]))/48
        #print(paste("50%: ", cov50), sep = "")
        #print(paste("90%: ", cov90), sep = "")
        
        list_results[[iseries]][[iday]] <- list(cov50, cov90)
        
      }# ALGO
    }# DAY
  } # iseries
 
  u <- sapply(list_results, function(list_res_iseries){
      sapply(list_res_iseries, function(list_days){
        simplify2array(list_days)
      })
    }, simplify = "array")
  res <- apply(u, c(1, 3), mean) # average coverage over 92 days
  matplot(t(res))
  abline(h = c(50, 90))
  print( apply(u, c(1), mean) )
  
}# agg and bottom



u <- sapply(list_results[[1]], function(list_days){
  simplify2array(list_days)
})
apply(u, 1, mean)

