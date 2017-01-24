rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

do.agg <- T
alliseries <- c(1)

#mymfrow <- c(3, 2); 
idays <- seq(1, 92, by = 1)
tag <- "TAG"; 
#algorithms <- c("KD-IC-NML", "TBATS")
algorithms <- c("KD-IC-NML")
algorithms <- c("DYNREG")
only.future <- FALSE

savepdf(file.path(results.folder, paste("PLOT_forecasts_ALL", tag, sep = "") ))

for(iseries in alliseries){
  #savepdf(file.path(results.folder, paste("PLOT_forecasts_", iseries, "_", tag, sep = "") ))
  
  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  if(TRUE){
    ############ PLOT the time series ############
    par(mfrow = c(2, 1))
    nly <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    setlim <- c(min(demand, na.rm = T), max(demand, na.rm = T))
    for(y in seq(2009,2010))
    {
      id <- which(calendar$year == y)
      if(y == 2009)
      {
        dat <- ts(c(rep(NA,(365*48)-length(id)), demand[id]), f = 365*48, s = y)
      }else{
        dat <-  ts(demand[id], f = 365*48, s = y)
      }
      
      nb_children <- "?"
      
      mymain <- paste(y, " - ", idseries, " (", nb_children, ")", sep = "")
      
      plot.ts(dat, main = mymain, ylim = setlim, xlab = "", ylab = "Demand (kW)", type = "n", xaxt = "n", xlim = y + c(0.035,.965))
      axis(at=y-1/48+(1:12)/12,label=month.abb,side=1,tck=0)
      abline(v=time(dat)[c(1,cumsum(nly)*48)],col="lightgoldenrod")
      lines(dat)
    }
    ######
  }
  par(mfrow = c(2, 2))
  list_load <- vector("list", length(algorithms))
  for(ialgo in seq_along(algorithms)){
    
    algo <- algorithms[ialgo]
    algo_load <- algo
    
    res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
    load(res_file)

    if(algo_load == "KD-IC-NML"){
      list_load[[ialgo]] <- list(all_qf = all_qf, all_tau = all_tau, all_mf = all_mf) #res_testing
    }else if(algo_load == "TBATS" || algo_load == "DYNREG"){
      list_load[[ialgo]] <- list(all_qf = all_qf, all_mf = all_mf)
    }else if(algo_load == "Uncond"){
      list_load[[ialgo]] <- list(qFtest = qFtest, mFtest = mFtest)
    }
  }#algo
  
  
  for(iday in idays){
    print(iday)
    for(ialgo in seq_along(algorithms)){
      
      algo <- algorithms[ialgo]
      algo_load <- algo
      
      if(algo_load == "KD-IC-NML" || algo_load == "TBATS" || algo_load == "DYNREG"){
        
        all_qf <- list_load[[ialgo]]$all_qf
        all_mf <- list_load[[ialgo]]$all_mf
        mu_hat <- matrix(unlist(all_mf), ncol = 48, byrow = T)
        
        if(algo_load == "KD-IC-NML"){
          all_tau <- list_load[[ialgo]]$all_tau
          
          qf_allhours <- sapply(seq(48), function(hour){
            invcdf <- approxfun(all_tau[[iday]][, hour], all_qf[[iday]][, hour], rule = 2)
            qf <- invcdf(alphas)
            qf
          })
        
        }else if(algo_load == "TBATS" || algo_load == "DYNREG"){
          qf_allhours <- sapply(seq(48), function(hour){
            qf <- all_qf[[iday]][, hour]
          })
        }
      }else if(algo_load == "Uncond"){
        qFtest <- list_load[[ialgo]]$qFtest
        mFtest <- list_load[[ialgo]]$mFtest
        
        qf_allhours <- qFtest
        mu_hat <- matrix(mFtest, ncol = 48, byrow = T)
      }

      rownames(qf_allhours) <- paste(alphas*100, "%", sep = "")
      
      future <- demand[test$id[(iday - 1) * 48 + seq(1, 48)]]
      subalphas    <- c("5%", "25%", "75%", "95%")
      #subalphas    <- c("1%", "25%", "75%", "99%")
      
      mymain <- paste(algo, " - ", 
                        seq_testing_interval[(iday - 1) * 48 + 1], " - ",
                        abbr.dweek[calendar$dweek[test$id[1] + (iday - 1) * 48]],
                        sep = "")
        
        myYLIM <- c(0, max(c(future, qf_allhours[subalphas, ]), na.rm = T))	
        
        plotQF(qf_allhours, future, subalphas, id = seq(48), only.future = only.future,
               main = mymain, xlab = "Time", ylab = "Electricity demand", xaxt='n', cex.lab = 1.2, ylim = myYLIM)
        axis(1, labels = tday, at = seq(1, 48))
        lines(mu_hat[iday, ], col = "red")

    }# ALGO
    
  }# DAY
  
  #dev.off()
} # iseries
dev.off()
