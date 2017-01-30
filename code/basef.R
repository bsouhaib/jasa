rm(list = ls())
print(base::date())
args = (commandArgs(TRUE))
if(length(args) == 0){
  #hierarchy <- "geo"
  #ncores <- 2
  algo <- c("KD-IC-NML")
  #algo <- c("TBATS")
  #algo <- "DYNREG"
    
  do.agg <- F
  alliseries <- 486
}else{
  
  for(i in 1:length(args)){
    print(args[[i]])
  }
  
  algo <- args[[1]]
  do.agg <- as.logical(args[[2]])
  alliseries <- NULL
  for(i in seq(3, length(args))){
    alliseries <- c(alliseries, as.numeric(args[[i]]))
  }
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
library(abind)
library(glmnet)

load(file.path(work.folder, "myinfo.Rdata"))

algos_allowed <- c("Uncond", "KD-IC-NML", "DYNREG")
stopifnot(algo %in% algos_allowed)

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

  
  #for(algo in algorithms){
  
  print(algo)
  res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
  dir.create(file.path(basef.folder, algo), showWarnings = FALSE)
 
  if(algo == "DYNREG"){
    
    do.logtrans <- T
    only.resid <- FALSE

    fourier.series = function(t,terms,period)
    {
      n = length(t)
      X = matrix(NA, nrow=n, ncol=2*terms)
      for(i in 1:terms)
      {
        X[,2*i-1] = sin(2*pi*i*t/period)
        X[,2*i]   = cos(2*pi*i*t/period)
      }
      colnames(X) = paste(c("S","C"),rep(1:terms,rep(2,terms)),sep="")
      return(X)
    }
    
    backtransform_log <- function(x, fvar){
      exp(x) * (1 + 0.5 * fvar)
    }
    
    p1 <- 48
    p2 <- 336
    max_k1 <- p1/2
    max_k2 <- p2/2
    
    #X1 <- fourier(y, K = c(max_k1, max_k2))	
    #X2 <- fourier.series(tyear, 4, 365.25)
    #X <- cbind(X1, X2)
    
    tyear <- calendar$tyear

    if(do.logtrans){
      my_ts <- log(demand)	     
    }else{
      my_ts <- demand
    }
    
    y <- msts(my_ts, seasonal.periods = c(p1, p2))  
    X1 <- fourier(y, K = c(max_k1, max_k2))	
    X2 <- fourier.series(tyear, 4, 365.25)
    X <- cbind(X1, X2)

    ids_future <- test$id
    nb_futuredays <- length(seq_testing_interval)/48
    
    model_arima <- model_fourier <- model_var <- NULL
    all_qf <- all_mf <- all_sd <- vector("list", nb_futuredays)
    mydays <- seq(1, nb_futuredays)
    
    for(id_future_day in mydays){
      print(id_future_day)
      #print(base::date())
      
      if(id_future_day == 1){
        ids_past   <-  learn$id
        n_past_obs <- length(ids_past)
      }else{
        n_past_obs <- n_past_obs_tbats
        ids_past   <- tail(learn$id, n_past_obs)
      }
      
      offset_nhours <- (id_future_day - 1) * 48
      ids_future_hours <- ids_future[offset_nhours + seq(1, 48)] 
      
      if(offset_nhours > 0){
        ids_past_actual <- c(ids_past, ids_future)[offset_nhours + seq(n_past_obs)]
      }else{
        ids_past_actual <- ids_past
      }
      
      ypast <- as.numeric(my_ts[ids_past_actual])
      
      #y <- msts(y, seasonal.periods = c(48, 336))     
      #tyear <- calendar$tyear[ids_past_actual]
      
      Xpast   <- X[ids_past_actual, ]
      Xfuture <- X[ids_future_hours, ]
      X2past   <- X2[ids_past_actual, ]
      X2future <- X2[ids_future_hours, ]
      
      do.fitting <- (id_future_day - 1) %% 7 == 0
      if(do.fitting){
        print("fitting")
        res_cv_mufourier <- cv.glmnet(x = Xpast, y = ypast, nfolds = 3, alpha = 1)
        best_lambda_mufourier <- res_cv_mufourier$lambda[which.min(res_cv_mufourier$cvm)]
        model_fourier <- glmnet(y = ypast, x = Xpast, alpha = 1, lambda = best_lambda_mufourier)
      }
      mu_fourier <- as.numeric(predict(model_fourier, Xpast))
      efourier <- ypast - mu_fourier
      log_efourier_squared <- as.numeric(log(efourier^2))
      
      if(do.fitting){
        res_cv_var <- cv.glmnet(X2past, log_efourier_squared, nfolds = 3, alpha = 0)
        best_lamnda <- res_cv_var$lambda[which.min(res_cv_var$cvm)]
        model_var <- glmnet(y = log_efourier_squared, x = X2past, alpha = 0, lambda = best_lamnda)
      }
      var_hat <- as.numeric(predict(model_var, X2past))
      var_hat <- exp(var_hat)
      efourier_scaled <- (ypast - mu_fourier)/sqrt(var_hat)
      
      #stop("done")
      
      if(do.fitting){
        model_arima <- auto.arima(efourier_scaled, seasonal=FALSE)
        
        if(id_future_day == 1)
        {
          #mu_arima <- fitted(model_arima)
          #mu_hat <- as.numeric(mu_fourier + mu_arima * sqrt(var_hat))
          #if(do.logtrans){
          #  mu_hat <- backtransform_log(mu_hat, var_hat) 
          # is it the right variance ???? !!
          #} 
          #e_residuals <- ypast - mu_hat
          
          e_residuals <- resid(model_arima)
          #stop("done")
          
          dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
          resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
          save(file = resid_file, list = c("e_residuals"))
        } 
      }else{
        
       model_arima <- Arima(efourier_scaled, model = model_arima)
        #mu_hat <- as.numeric(mu_fourier + mu_arima * sqrt(var_hat)) 
      }

      if(!only.resid){
        res <- forecast(model_arima, h = 48, level = 95)
        mu_hat_escaled <- res$mean
        sd_hat_escaled <- (res$upper-res$lower)/1.96/2
        qf_escaled <- matrix(NA, nrow = length(alphas), ncol = 48)
        for(h in seq(48))
          qf_escaled[,h] <- qnorm(alphas, mu_hat_escaled[h], sd_hat_escaled[h])
        
        mu_fourier_pred <- predict(model_fourier, Xfuture)
        var_pred <- exp(predict(model_var, X2future))
        sd_pred <- sqrt(as.numeric(var_pred))
        
        qf     <- t(t(qf_escaled) * sd_pred + as.numeric(mu_fourier_pred))
        mu_hat <- as.numeric(mu_hat_escaled * sd_pred + as.numeric(mu_fourier_pred))
        #matplot(, type = 'l')
        
        if(do.logtrans){
          qf <- exp(qf)
          mu_hat <- backtransform_log(mu_hat, var_pred)
        }
        
        all_qf[[id_future_day]] <- qf
        all_mf[[id_future_day]] <- mu_hat
      }

    } # test days	
    
    if(!only.resid)
      save(file = res_file, list = c("all_qf", "all_mf"))
    
  }else if(algo == "Uncond"){
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
    
    ### LEARNING
    res_learning <- predictkde("learning")
    
    results_crps <- sapply(res_learning$results, function(list_vectors){
      sapply(list_vectors, function(vector){identity(vector)
        })
      }, simplify = "array") 
    ic_days <- res_learning$ic_days
    
    idbest_bandwiths <- NULL
    for(ic in seq(3)){
      err <- apply(results_crps[, , which(ic_days == ic)], 1, median)
      idbest_bandwiths <- c(idbest_bandwiths, which.min(err))
    }
    selected_bandwiths_ic <- res_learning$bandwiths[idbest_bandwiths]
    
    ### TESTING
    res_testing <- predictkde("testing", selected_bandwiths = selected_bandwiths_ic)
    
    # all_crps <- getItem(res_testing$results, "crps")
    all_qf  <- getfromlist(res_testing$results, "qtauhat")
    all_tau <- getfromlist(res_testing$results, "tauhat")
    all_mf  <- getfromlist(res_testing$results, "mu_hat")
    
    save(file = res_file, list = c("all_qf", "all_tau", "all_mf"))
    
    ### IN SAMPLE INFO
    res_insample_info <- predictkde("insample_info", selected_bandwiths = selected_bandwiths_ic)

    # residuals
    all_residuals <- getfromlist(res_insample_info$results, "residuals")
    e_residuals_unscaled <- unlist(all_residuals)
    all_var <- getfromlist(res_insample_info$results, "var_hat")
    all_varhat <- unlist(all_var)
    e_residuals <- e_residuals_unscaled/sqrt(all_varhat)
    
    dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
    resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = resid_file, list = c("e_residuals"))
    
    # extract cdfs
    all_qf_insample  <- getfromlist(res_insample_info$results, "qtauhat")
    all_tau_insample <- getfromlist(res_insample_info$results, "tauhat")
    
    insamplecdf_file <- file.path(insample.folder, algo, paste("insamplecdf_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = insamplecdf_file, list = c("all_qf_insample", "all_tau_insample"))
    
  }else if(algo %in% c("TBATS", "BATS")){
    
    only.resid <- FALSE
    
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
   
      #ids_past   <- tail(learn$id, (31 + 28 + 31 + 30)*48)
      ids_future <- test$id
      nb_futuredays <- length(seq_testing_interval)/48
      
      #res_file   <- file.path(basef.folder, algo, paste("results_testing_", idseries, "_", algo, ".Rdata", sep = "")) 
      #model_file <- file.path(basef.folder, algo, paste("model_testing_", idseries, "_", algo, ".Rdata", sep = "")) 
      
    #}else{
    #  stop("ERROR !")
    #}
    
    model <- NULL
    
    all_qf <- all_mf <- all_sd <- vector("list", nb_futuredays)
    
    mydays <- seq(1, nb_futuredays)
    if(only.resid){
      mydays <- 1
    }
    
    backtransform_log <- function(x, fvar){
      exp(x) * (1 + 0.5 * fvar)
    }
    
    for(id_future_day in mydays){
      print(id_future_day)
      #print(base::date())

      if(id_future_day == 1){
        ids_past   <-  learn$id
        n_past_obs <- length(ids_past)
      }else{
        n_past_obs <- n_past_obs_tbats
        ids_past   <- tail(learn$id, n_past_obs)
      }
      
      offset_nhours <- (id_future_day - 1) * 48
      
      #ids_future_hours <- ids_future[1 + offset_nhours] + seq(0, 47)
      ids_future_hours <- ids_future[offset_nhours + seq(1, 48)] 
      
      if(offset_nhours > 0){
        #ids_past_actual <- c(tail(ids_past, -offset_nhours), head(ids_future, offset_nhours))
        ids_past_actual <- c(ids_past, ids_future)[offset_nhours + seq(n_past_obs)]
      }else{
        ids_past_actual <- ids_past
      }
      
      y <- demand[ids_past_actual]
      
      if(do.agg){
        seasonal.periods <- c(48, 336)
      }else{
        seasonal.periods <- c(336) # c(48) !!!!!!!!!!!!!!!!!!!
      }
     
      do.logtrans <- F
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
        #yfitted <- exp(fitted(model))
        e_residuals <- exp(model$y) - backtransform_log(fitted(model), model$variance)
      }else{
        #yfitted <- fitted(model)
        e_residuals <- resid(model)
      }
      
      if(id_future_day == 1)
      {
        dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
        resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        save(file = resid_file, list = c("e_residuals"))
      } 
      
      #list_yfitted[[id_future_day]] <- yfitted
      #if(id_future_day %in% c(1, seq(10, 90, 10)))
      #{
        #print(date())
        #print("Writing")
        #model_file <- file.path(basef.folder, algo, paste("model_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        #save(file = model_file, list = c("model"))
      #}
      
      if(!only.resid){
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
          #mu_hat <- exp(res$mean)  
          mu_hat <- backtransform_log(res$mean, sd_hat^2)
        }
        all_mf[[id_future_day]] <- mu_hat
      }
      
      
      
    } # test days	
    
    if(!only.resid)
    save(file = res_file, list = c("all_qf", "all_mf"))
    
  } # (T)BATS test
}
