rm(list = ls())
print(base::date())
args = (commandArgs(TRUE))
if(length(args) == 0){
  #hierarchy <- "geo"
  #ncores <- 2
  algo <- c("KD-IC-NML")
  algo <- c("TBATS")

  do.agg <- TRUE
  alliseries <- 1
  
}else{
  
  for(i in 1:length(args)){
    print(args[[i]])
  }
  
  #hierarchy <- args[[1]]
  #ncores <- as.numeric(args[[1]])
  algo <- args[[1]]
  do.agg <- as.logical(args[[2]])
  alliseries <- NULL
  for(i in seq(3, length(args))){
    alliseries <- c(alliseries, as.numeric(args[[i]]))
  }

  #algorithms <- NULL
  #for(i in seq(4, length(args))){
  #	algorithms <- c(algorithms, args[[i]])
  #}
  
  
}

print(algo)

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")

library(parallel)
library(fBasics)
library(msm)
library(gtools)
library(forecast)

load(file.path(work.folder, "myinfo.Rdata"))

algos_allowed <- c("Uncond", "KD-IC-TRC", "KD-IC-NML", "TBATS", "BATS")
stopifnot(algo %in% algos_allowed)

#res <- mclapply(alliseries, function(iseries){
#res <- mclapply(seq(2), function(iseries){
for(iseries in alliseries){

  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  #if(iseries == 4)
  #{
  #  demand <- demand + jitter(demand, amount = 0)
  #}
  
  do.scaling <- FALSE
  if(do.scaling){
    demand <- demand / max(demand)
  }
  
  
  set.seed(1986)
  u1 <- runif(10000)
  u2 <- runif(10000)
  
  #for(algo in algorithms){
  
  print(algo)
  res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
  dir.create(file.path(basef.folder, algo), showWarnings = FALSE)
 
  
  if(algo == "Uncond"){
    qFlearn <- quantile(demand[learn$id], alphas)
    qFtest <- matrix(rep(qFlearn, length(test$id)), ncol = length(test$id))
    
    mFlearn <- mean(demand[learn$id])
    mFtest <- rep(mFlearn, length(test$id))
    save(file = res_file, list = c("qFtest", "mFtest"))
    
  }else if(grepl("KD-D", algo) || grepl("KD-IC", algo)){
    
    
    if(grepl("TRC", algo)){
      mykernel <- "truncated"
    }else if(grepl("NML", algo)){
      mykernel <- "normal"
    }else if(grepl("LNL", algo)){
      mykernel <- "lognormal"
    }else{
      mykernel <- "normal"
    }
    
    #print(mykernel)
    ### LEARNING
    res_learning <- predictkde("learning")
    
    # Best bandwith for dayhours
    crps_dayhours <- getfromlist(res_learning$res_dayhours, "crps")
    avg_crps_dayhours <- apply(crps_dayhours, 1, mean)
    id_best_dayhours <- which.min(avg_crps_dayhours)
    bandwiths_dayhours_best <- res_learning$bandwiths_dayhours[id_best_dayhours]
    
    # Best bandwith for nighthours
    crps_nighthours <- getfromlist(res_learning$res_nighthours, "crps")
    avg_crps_nighthours <- apply(crps_nighthours, 1, mean)
    id_best_nighthours <- which.min(avg_crps_nighthours)
    bandwiths_nighthours_best <- res_learning$bandwiths_nighthours[id_best_nighthours]
    
    
    ### TESTING
    res_testing <- predictkde("testing", 
                              bandwiths_nighthours = bandwiths_nighthours_best, 
                              bandwiths_dayhours = bandwiths_dayhours_best)
    
    ### RESIDUALS
    res_residuals <- predictkde("residuals", 
                              bandwiths_nighthours = bandwiths_nighthours_best, 
                              bandwiths_dayhours = bandwiths_dayhours_best)
    
    save(file = res_file, list = c("res_learning", "res_testing", "res_residuals"))
  }else if(algo %in% c("TBATS", "BATS")){
    
    
    if(algo == "TBATS"){
      modelfct <- tbats
    }else if(algo == "BATS"){
      modelfct <- bats
    }
    
    ####### IMPOSING LOG TRANSFORM ######	
    if(FALSE){	
      my.BoxCox.lambda <- function (x, lower = NULL, upper = NULL) 0.5;
      # where is it defined?
      # getAnywhere("BoxCox.lambda")
      # in package and in namespace
      
      unlockBinding("BoxCox.lambda", as.environment("package:forecast"))
      assign("BoxCox.lambda", my.BoxCox.lambda, "package:forecast")
      
      unlockBinding("BoxCox.lambda", getNamespace("forecast"))
      assign("BoxCox.lambda", my.BoxCox.lambda, getNamespace("forecast"))
      
      #getAnywhere("BoxCox.lambda")[1]
      #getAnywhere("BoxCox.lambda")[2]
    }
    ######## NO FITTING OF A NONSEASONAL MODEL ##############################
    my.bats <- function (x, use.box.cox, use.trend,
                         use.damped.trend, use.arma.errors,
                         use.parallel, num.cores,
                         bc.lower, bc.upper, ...) list(AIC = Inf);
    # where is it defined?
    # getAnywhere("bats")
    # in package and in namespace
    
    unlockBinding("bats", as.environment("package:forecast"))
    assign("bats", my.bats, "package:forecast")
    
    unlockBinding("bats", getNamespace("forecast"))
    assign("bats", my.bats, getNamespace("forecast"))
    ######################################
    
    
    #ids_past   <- learn$id
    #ids_future <- test$id		
    #nb_futuredays <- length(seq_testing_interval)/48
    
    #if(task == "learning"){
    #  ids_past   <- tail(train$id, (31 + 28 + 31)*48)
    #  ids_future <- validation$id
    #  nb_futuredays <- length(seq_validation_interval)/48
      
    #  res_file   <- file.path(basef.folder, algo, paste("results_learning_", idseries, "_", algo, ".Rdata", sep = "")) 
    #  model_file <- file.path(basef.folder, algo, paste("model_learning_", idseries, "_", algo, ".Rdata", sep = "")) 

    #}else if(task == "testing"){
      ids_past   <- tail(learn$id, (31 + 28 + 31 + 30)*48)
      ids_future <- test$id
      nb_futuredays <- length(seq_testing_interval)/48
      
      #res_file   <- file.path(basef.folder, algo, paste("results_testing_", idseries, "_", algo, ".Rdata", sep = "")) 
      #model_file <- file.path(basef.folder, algo, paste("model_testing_", idseries, "_", algo, ".Rdata", sep = "")) 
      
    #}else{
    #  stop("ERROR !")
    #}
    
    
    
    model <- NULL
    
    all_qf <- all_mf <- all_sd <- vector("list", nb_futuredays)
    
    for(id_future_day in seq(1, nb_futuredays)){
      #print(id_future_day)
      #print(base::date())
      offset_nhours <- (id_future_day - 1) * 48
      
      ids_future_hours <- ids_future[1 + offset_nhours] + seq(0, 47)
      
      if(offset_nhours > 0){
        ids_past_actual <- c(tail(ids_past, -offset_nhours), head(ids_future, offset_nhours))
      }else{
        ids_past_actual <- ids_past
      }
      
      y <- demand[ids_past_actual]
      
      #print(date())
      # 48-hours ahead forecasts
      
      
      #model <- modelfct(y, seasonal.periods = c(48, 336), use.trend = F, use.damped.trend = F, 
      #               use.arma.errors = T, max.p = 2, max.q = 2, 
      #               use.parallel = F, num.cores = 1,
      #               use.box.cox = F)
      
      #stop("done")
      stop("done")
      
      # x <- msts(taylor, seasonal.periods=c(48,336), start=2000+22/52)
      # f1 <- forecast(x, h = 270)
      
      # x <- msts(taylor, seasonal.periods = c(48, 336))
      # f2 <- forecast(x, h = 270)
      
      # taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
      # f3 <- forecast(taylor.lm, data.frame(fourier(taylor, K = c(3, 3), h = 270)))
      
      # x <- msts(y, seasonal.periods = c(48, 336))
      # fit <- Arima(y, order=c(2,0,1), xreg=fourier(y, K=4)) or fit <- auto.arima(x, seasonal=FALSE, xreg=fourier(x, K= c(2, 2) ))
      # e <- resid(fit)
      # plot(forecast(fit, h=48, xreg=fourier(x, K= c(2, 2), h = 48)), include = 48 *7)
      # plot(forecast(fit, h=2*m, xreg=fourier(y, K=4, h=2*m)))
      # 
      
      # x <- msts(y, seasonal.periods = c(48, 336))
      # fcast <- forecast(x, h = 48 * 6)
      
      if(do.agg){
        seasonal.periods <- c(48, 336)
      }else{
        seasonal.periods <- c(336) # c(48) !!!!!!!!!!!!!!!!!!!
      }
     
      do.logtrans <- T
      if(do.logtrans){
        my_ts <- log(y)	     
      }else{
        my_ts <- y
      }
      
      model <- modelfct(my_ts, 
                        seasonal.periods = seasonal.periods, 
                        use.trend = F, use.damped.trend = F, 
                        use.arma.errors = T, max.p = 2, max.q = 2, 
                        use.parallel = F, num.cores = 1,
                        use.box.cox = F)
      
      if(do.logtrans){
        yfitted <- exp(fitted(model))
      }else{
        yfitted <- fitted(model)
      }
      
      #list_yfitted[[id_future_day]] <- yfitted
      
      #if(id_future_day %in% c(1, seq(10, 90, 10)))
      #{
        #print(date())
        #print("Writing")
        #model_file <- file.path(basef.folder, algo, paste("model_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        #save(file = model_file, list = c("model"))
      #}
      
      res <- forecast(model, h = 48, level = 95)
      
      # mu
      mu_hat <- res$mean
      
      # sd
      sd_hat <- (res$upper-res$lower)/1.96/2
      #all_sd[[id_future_day]] <- sd_hat
      
      qf <- matrix(NA, nrow = length(alphas), ncol = 48)
      for(h in seq(48))
        qf[,h] <- qnorm(alphas, mu_hat[h], sd_hat[h])
        #qf[,h] <- qtnorm(alphas, mean= mu_hat[h], sd= sd_hat[h], lower=0, upper=Inf, lower.tail = TRUE, log.p = FALSE)
      
      if(do.logtrans){
        qf <- exp(qf)
      }
      all_qf[[id_future_day]] <- qf
      
      
      if(do.logtrans){
        mu_hat <- exp(res$mean)
      }
      all_mf[[id_future_day]] <- mu_hat
      
      
      
      
    } # test days	
    save(file = res_file, list = c("all_qf", "all_mf"))
    
  } # (T)BATS test
  
  #} # algorithms

}
#}, mc.cores = 2)
