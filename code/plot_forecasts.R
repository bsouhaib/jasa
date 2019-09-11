rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

#plot.permsamples <- FALSE
plot.permsamples <- FALSE
do.jasa <- TRUE

if(plot.permsamples){
  
  do.agg <- T
  algo.agg <- "DETS"
  algo.bottom  <- "KD-IC-NML"
  
  alliseries <- c(1)
  #alliseries <- sort(apply(Sagg, 1, sum), index = T, decreasing = T)$ix
  idays <- seq(1, 2, by = 1)
  idays <- 1
  algorithms <- c("INDEPBU", "PERMBU", "MINTshrink", "INDEPBU-MINTshrink", "PERMBU-MINTshrink", "BASE")
  agg_methods <- c("BASE", "INDEPBU", "PERMBU", "PERMBU-MINTshrink", "INDEPBU-MINTshrink", "MINTdiag", "MINTshrink")
  
  nbperjob <- 368
  
  QF_agg <- array(NA, c(M, length(algorithms), 48*2))
  allidtest <- seq(1, 48*2)
  idjob <- 1
  samples_job <- file.path(work.folder, "samples_agg", paste("samples_agg_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
  load(samples_job)
  
  list_samples_agg_nonull <- list_samples_agg[-which(sapply(list_samples_agg, is.null))]
  BIGARRAY <- sapply(seq_along(list_samples_agg_nonull),  function(i){list_samples_agg_nonull[[i]]}, simplify = 'array')
  QF_agg[, , allidtest] <- BIGARRAY[, alliseries, match(algorithms, agg_methods), ]
  
  mf_agg <- apply(QF_agg, c(2, 3), mean)
  qf_agg <- apply(QF_agg, c(2, 3), quantile, prob = taus)
  all_qf <- lapply(idays, function(iday){
    qf_agg[, , (iday - 1) * 48 + seq(48) ]
  })
  all_mf <- lapply(idays, function(iday){
    mf_agg[, (iday - 1) * 48 + seq(48) ]
  })
  
  #stop("done")
  
}else if(FALSE){
  do.agg <- T
  alliseries <- c(1)
  idays <- seq(1, 7, by = 1)
  algorithms <- c("NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB")
  
  
  algo.agg <- "DYNREG"
  algo.bottom  <- "KD-IC-NML"
  
  #agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MEANCOMB")
  
  agg_methods <- c("BASE", "NAIVEBU", "PERMBU", "PERMBU-MINT", "PERMBU-MCOMB", "PERMBU-MCOMBRECON", "PERMBU-MCOMBUNRECON")
  nbperjob <- 69
  
  QF_agg <- array(NA, c(M, length(algorithms), 69 * 5))
  for(idjob in c(1, 2, 3, 4, 5)){
    print(idjob)
    allidtest <- (idjob - 1) * nbperjob + seq(nbperjob) 
    
    samples_job <- file.path(work.folder, "samples_agg", paste("samples_agg_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
    load(samples_job)
    #  samples_agg <- array(NA, c(M, n_agg, length(agg_methods)))
    list_samples_agg_nonull <- list_samples_agg[-which(sapply(list_samples_agg, is.null))]
    
    BIGARRAY <- sapply(seq_along(list_samples_agg_nonull),  function(i){list_samples_agg_nonull[[i]]}, simplify = 'array')
    
    QF_agg[, , allidtest] <- BIGARRAY[, alliseries, match(algorithms, agg_methods), ]
  }
  
  mf_agg <- apply(QF_agg, c(2, 3), mean)
  qf_agg <- apply(QF_agg, c(2, 3), quantile, prob = taus)
  all_qf <- lapply(idays, function(iday){
    qf_agg[, , (iday - 1) * 48 + seq(48) ]
  })
  all_mf <- lapply(idays, function(iday){
    mf_agg[, (iday - 1) * 48 + seq(48) ]
  })
  
}else{
  do.agg <- F
  alliseries <- seq(200) #c(1267)
  #algorithms <- c("DYNREG")
  algorithms <- c("KD-IC-NML")
  #algorithms <- c("DETS")

  idays <- seq(1, 92, by = 1)
  idays <- 1
  
  
  if(do.jasa){
    series_isagg <- c(TRUE, FALSE) # actual
    alliseries <- c(1, 34) # actual
    idays <- c(1) # actual
    idays <- c(11) # bettter coverage
    
    #alliseries <- c(1, 1453)
    #alliseries <- c(1, 50)
    #idays <- c(90)
    # for agg: 3
    # for bottom: 35, 75, 
  }
}

only.future <- FALSE



if(do.jasa){
  tag <- "example"
  savepdf(file.path(results.folder, paste("PLOT_forecasts_", tag, sep = "")), height = 27 * 0.25, width = 21)
  par(mfrow = c(1, 2))
}else{
  tag <- "allmethods"
  savepdf(file.path(results.folder, paste("PLOT_forecasts_", tag, sep = "") ))
}

for(iseries in alliseries){
  #savepdf(file.path(results.folder, paste("PLOT_forecasts_", iseries, "_", tag, sep = "") ))
  
  if(do.jasa){
    do.agg <- series_isagg[which(iseries == alliseries)]
    if(do.agg){
      algorithms <- "DETS"
    }else{
      algorithms <- "KD-IC-NML"
    }
  }
  
  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  if(FALSE){
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
      
      #mymain <- paste(y, " - ", idseries, " (", nb_children, ")", sep = "")
      mymain <- paste(y, " - ", idseries, " (", nb_children, ")", sep = "")
      
      plot.ts(dat, main = mymain, ylim = setlim, xlab = "", ylab = "Demand (kW)", type = "n", xaxt = "n", xlim = y + c(0.035,.965))
      axis(at=y-1/48+(1:12)/12,label=month.abb,side=1,tck=0)
      abline(v=time(dat)[c(1,cumsum(nly)*48)],col="lightgoldenrod")
      lines(dat)
    }
    ######
  }
  
  if(!do.jasa){
    #par(mfrow = c(2, 2))
    par(mfrow = c(2, 3))
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
  
  
  for(iday in idays){
    
    day_min <- Inf
    day_max <- -Inf
    for(ialgo in seq_along(algorithms)){
      day_min <- pmin(day_min, min(list_load[[ialgo]]$all_qf[[iday]]))
      day_max <- pmax(day_max, max(list_load[[ialgo]]$all_qf[[iday]]))
    }

    print(iday)
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
      #subtaus    <- c("1%", "25%", "75%", "99%")
      
      #mymain <- paste(algo, " - ", 
      #                  seq_testing_interval[(iday - 1) * 48 + 1], " - ",
      #                  abbr.dweek[calendar$dweek[test$id[1] + (iday - 1) * 48]],
      #                  sep = "")
      #mymain <- ifelse(algo == "KD-IC-NML", "KDE", ifelse(algo == "DETS", "HWT-ETS", algo))
      mymain <- ifelse(algo == "KD-IC-NML", "Individual smart meter", ifelse(algo == "DETS", "Top aggregated series", algo))
      
      #myYLIM <- c(0, max(c(future, qf_allhours[subtaus, ]), na.rm = T))	
        myYLIM <- c(day_min, day_max)
        
        # COVERAGE
        
        # qf_allhours["75%", ]
        # qf_allhours["25%", ]
        # future > qf_allhours["25%", ] & future < qf_allhours["75%", ]
        
        cov50 <- 100 * length(which(future >= qf_allhours["25%", ] & future <= qf_allhours["75%", ]))/48
        cov90 <- 100 * length(which(future >= qf_allhours["5%", ] & future <= qf_allhours["95%", ]))/48
        print(paste("50%: ", cov50), sep = "")
        print(paste("90%: ", cov90), sep = "")
        
        
        plotQF(qf_allhours, future, subtaus, id = seq(48), only.future = only.future,
               main = mymain, xlab = "Time of day", ylab = "Consumption (kWh)", xaxt='n', cex.lab = 1.2, ylim = myYLIM)
        
        #axis(1, labels = tday, at = seq(1, 48))
        itday <- c(1, seq(8, 48, by = 8))
        axis(1, labels = tday[itday], at = itday, cex.axis=0.9)
        
        lines(mu_hat[iday, ], col = "red")

    }# ALGO
    
    #if(!do.jasa){
      #plot.new()
      #plot.new()
    #}
  }# DAY
  
  #dev.off()
} # iseries
dev.off()
