rm(list = ls())
print(base::date())
args = (commandArgs(TRUE))
if(length(args) == 0){
  #hierarchy <- "geo"
  #ncores <- 2
  #algo <- c("KD-IC-NML")
  #algo <- c("TBATS")
  #algo <- "DYNREG"
  algo <- "DETS"
    
  do.agg <- F
  alliseries <- c(354)
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

algos_allowed <- c("Uncond", "KD-IC-NML", "DYNREG", "DETS")
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
 
  if(algo == "DETS"){
    
    do.logtrans <- FALSE
    if(do.logtrans){
      my_ts <- log(demand)	     
    }else{
      my_ts <- demand
    }
    
    ids_future <- test$id
    nb_futuredays <- length(seq_testing_interval)/48
    
    all_qf <- all_mf <- all_sd <- all_mfsample <- vector("list", nb_futuredays)
    mydays <- seq(1, nb_futuredays)
    
    for(id_future_day in mydays){
      print(id_future_day)
      print(base::date())
      
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
      
      do.optimize <- (id_future_day - 1) %% 7 == 0
      
      
        # initialization
        a <- 1/336 * (mean(ypast[seq(336)]) - mean(ypast[336 + seq(336)]))
        b <- mean(diff(ypast[seq(336)]))
        T_0 <- (a+b)/2
        l_start <- mean(ypast[seq(2 * 336)]) - 336.5 * T_0
        
        # days
        nb_obs <- 7 * m_1
        indices <- seq(nb_obs)
        smoothed_line <- ma(ypast[indices], m_1)
        #indices <- seq(m_1/2 + 1, nb_obs - m_1/2)
        x <- ypast[indices] - smoothed_line[indices]
        mat <- matrix(x, ncol = 48, byrow = T)
        D <- apply(mat, 2, mean, na.rm = T)
        D <- D - mean(D)
        
        # weeks
        nb_weeks <- 4
        indices <- seq(nb_weeks * m_2)
        smoothed_line <- ma(ypast[indices], m_2)
        x <- ypast[indices] - smoothed_line[indices] - rep(D, nb_weeks * 7)
        mat <- matrix(x, ncol = 336, byrow = T)
        W <- apply(mat, 2, mean, na.rm = T)
        W <- W - mean(W)
        
        e_0 <- rep(0, m_2)
        l_0 <- rep(l_start, m_2)
        d_0 <- rep(D, 7)
        w_0 <- W
        
      ###  
      if(do.optimize){  
        N <- 100
        THETA <-  matrix(runif(N * 4), ncol = 4)
        E <- sapply(seq(nrow(THETA)), function(i){ 
          #print(i)
          func_to_optimize(THETA[i, ], y = ypast, e_0 = e_0, l_0 = l_0, d_0 = d_0, w_0 = w_0, do.forecast = FALSE)
        })
        id <- sort(E, index = T)$ix[1]
        res_optim <- optim(THETA[id, ], fn = func_to_optimize, y = ypast, e_0 = e_0, l_0 = l_0, d_0 = d_0, w_0 = w_0, do.forecast = F,
                           method = "L-BFGS-B", lower = 0, upper = 1)
      }
        
      #if(id_future_day == 23)
      #  stop("done")
        
      obj_forecast <- iterate(res_optim$par, ypast, e_0, l_0, d_0, w_0, do.forecast = T)
      
      #matplot(t(obj_forecast$qf), type = 'l', lty = 1)
      #points(demand[test$id[seq(48)]], lwd = 2)  
      
      
      if(id_future_day == 1){
        dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
        # insample mean
        all_mu <- obj_forecast$yhat
        insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        save(file = insample_condmean_file, list = c("all_mu"))
        
        # residuals COPULA
        e_residuals <- obj_forecast$residuals
        resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        save(file = resid_file, list = c("e_residuals"))
        
        # residuals MINT
        residuals_MINT <- obj_forecast$residuals
        resid_MINT_file <- file.path(insample.folder, algo, paste("residuals_MINT_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
        save(file = resid_MINT_file, list = c("residuals_MINT"))
      }

      all_mf[[id_future_day]] <- obj_forecast$mf
      all_qf[[id_future_day]] <- obj_forecast$qf
      
      all_mfsample[[id_future_day]] <- obj_forecast$mfsample
      
    }
    list_save <- c("all_qf", "all_mf", "all_mfsample")
    save(file = res_file, list = list_save)
    
  }else if(algo == "DYNREG"){
    
    do.logtrans <- TRUE
    do.condvar <- FALSE
    only.insample <- FALSE

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
    all_qfe <- all_mu_z_fourier <- all_sd_z_fourier <- vector("list", nb_futuredays)
    mydays <- seq(1, nb_futuredays)
    
    for(id_future_day in mydays){
      #print(id_future_day)
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
      #do.fitting <- TRUE
      
      if(do.fitting){
        #print("fitting")
        cv_mu_z_fourier <- cv.glmnet(y = ypast, x = Xpast, nfolds = 3, alpha = 1)
        best_lambda_mu_z_fourier <- cv_mu_z_fourier$lambda[which.min(cv_mu_z_fourier$cvm)]
        model_mu_z_fourier <- glmnet(y = ypast, x = Xpast, alpha = 1, lambda = best_lambda_mu_z_fourier)
      }
      mu_z_fourier <- as.numeric(predict(model_mu_z_fourier, Xpast))
      efourier <- ypast - mu_z_fourier
      
      if(do.condvar){
        log_efourier_squared <- as.numeric(log(efourier^2))
        if(do.fitting){
          cv_var_z_fourier <- cv.glmnet(X2past, log_efourier_squared, nfolds = 3, alpha = 0)
          best_lamnda_var_z_fourier <- cv_var_z_fourier$lambda[which.min(cv_var_z_fourier$cvm)]
          model_var_z_fourier <- glmnet(y = log_efourier_squared, x = X2past, alpha = 0, lambda = best_lamnda_var_z_fourier)
        }
        var_z_fourier <- exp(as.numeric(predict(model_var_z_fourier, X2past)))
        efourier_scaled <- efourier/sqrt(var_z_fourier)
      }else{
        efourier_scaled <- efourier
      }
      
      if(do.fitting){
        model_arima <- auto.arima(efourier_scaled, seasonal=FALSE)
        if(id_future_day == 1)
        {
          # insample residuals COPULA
          e_residuals <- resid(model_arima)

          dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
          resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
          save(file = resid_file, list = c("e_residuals"))
          
          # insample mean and quantiles
          mu_e <- fitted(model_arima)
          var_e <- model_arima$sigma2
          
          qf_e <- sapply(seq_along(mu_e), function(i){
            qnorm(taus, mean = mu_e[i], sd = sqrt(var_e))
          })
          
          if(do.condvar){
            mu_z <- as.numeric(mu_z_fourier + mu_e * sqrt(var_z_fourier))
            qf_z <- t(as.numeric(mu_z_fourier) + t(qf_e) * sqrt(var_z_fourier))
            var_z <- var_e * var_z_fourier
          }else{
            mu_z <- as.numeric(mu_z_fourier + mu_e)
            qf_z <- t(as.numeric(mu_z_fourier) + t(qf_e))
            var_z <- var_e
          }
          
          if(do.logtrans){
            mu_y <- backtransform_log(mu_z, var_z)
            qf_y <- exp(qf_z)
          }else{
            mu_y <- mu_z
            qf_y <- qf_z 
          }
          all_qf_insample <- qf_y
          #all_qfe_insample <- qf_e
          
          # insample mean
          all_mu <- mu_y
          insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
          save(file = insample_condmean_file, list = c("all_mu"))
          
          # residuals MINT
          residuals_MINT <- demand[learn$id] - all_mu 
          resid_MINT_file <- file.path(insample.folder, algo, paste("residuals_MINT_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
          save(file = resid_MINT_file, list = c("residuals_MINT"))
            
          #insample_quantile_file <- file.path(insample.folder, algo, paste("quantiles_", idseries, "_", algo, "_", id_future_day, ".Rdata", sep = "")) 
          #save(file = insample_quantile_file, list = c("all_qf_insample"))
          #save(file = insample_quantile_file, list = c("all_qfe_insample", "var_z_fourier", "mu_z_fourier"))
          
           
          #if(only.insample){
          #  stop("INSAMPLE DONE.")
          #}
          
        } 
      }else{
        model_arima <- Arima(efourier_scaled, model = model_arima)
      }

      
        f_arima <- forecast(model_arima, h = 48, level = 95)
        mu_e <- f_arima$mean
        sd_e <- (f_arima$upper-f_arima$lower)/1.96/2
        qf_e <- matrix(NA, nrow = length(taus), ncol = 48)
        for(h in seq(48))
          qf_e[,h] <- qnorm(taus, mu_e[h], sd_e[h])
        
        mu_z_fourier  <-  predict(model_mu_z_fourier, Xfuture) 
        if(do.condvar){
          var_z_fourier <- exp(predict(model_var_z_fourier, X2future))
          sd_z_fourier  <- sqrt(as.numeric(var_z_fourier)) 
        
          qf_z     <- t(t(qf_e) * sd_z_fourier + as.numeric(mu_z_fourier))
          mu_z <- as.numeric(mu_e * sd_z_fourier + as.numeric(mu_z_fourier))
          var_z <-  sd_e^2 * sd_z_fourier^2
        }else{
          qf_z     <- t(t(qf_e) + as.numeric(mu_z_fourier))
          mu_z <- as.numeric(mu_e + as.numeric(mu_z_fourier))
          var_z <-  sd_e^2
        }
        
        if(do.logtrans){
          mu_y <- backtransform_log(mu_z, var_z)
          qf_y <- exp(qf_z)
        }else{
          mu_y <- mu_z
          qf_y <- qf_z
        }
        all_mf[[id_future_day]] <- mu_y
        all_qf[[id_future_day]] <- qf_y
        
        #all_qfe[[id_future_day]] <- qf_e
        #if(do.condvar){
          #all_sd_z_fourier[[id_future_day]] <- sd_z_fourier
        #}
        #all_mu_z_fourier[[id_future_day]] <- mu_z_fourier

    } # test days	
  
    list_save <- c("all_qf", "all_mf")
    # list_save <- c("all_qf", "all_mf", "all_qfe", "all_sd_z_fourier", "all_mu_z_fourier")
    save(file = res_file, list = list_save)

  }else if(algo == "Uncond"){
    qFlearn <- quantile(demand[learn$id], taus)
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
    #stop("done")
    
    #results_crps <- sapply(res_learning$results, function(list_vectors){
    #  sapply(list_vectors, function(vector){
    #    identity(vector)
    #    })
    #  }, simplify = "array") 
    
    #ic_days <- res_learning$ic_days
    
    #idbest_bandwiths <- NULL
    #for(ic in seq(3)){
    #  err <- apply(results_crps[, , which(ic_days == ic)], 1, median)
    #  idbest_bandwiths <- c(idbest_bandwiths, which.min(err))
    #}
    #selected_bandwiths_ic <- res_learning$bandwiths[idbest_bandwiths]
    
    results_crps <- sapply(res_learning$results, function(list_vectors){
        sapply(list_vectors, function(list_two){
          sapply(list_two, function(vector){
            identity(vector)
          }, simplify = "array")
          
          }, simplify = "array")
        }, simplify = "array") 
    
    ic_days <- res_learning$ic_days
    
    idbest_bandwiths <- idbest_lambda <- NULL
    for(ic in seq(3)){
      err <- apply(results_crps[, , , which(ic_days == ic)], c(1, 3), median)
      idbest <- which(err == min(err), arr.ind = T)
      #print(err)
      #print(idbest)
      idbest_bandwiths <- c(idbest_bandwiths, idbest[1, 1])
      idbest_lambda    <- c(idbest_lambda, idbest[1, 2])
    }
    selected_bandwiths_ic <- res_learning$bandwiths[idbest_bandwiths]
    selected_lambdas_ic <- res_learning$lambdas[idbest_lambda]
    
    # boxplot(apply(results_crps[, , which(ic_days == ic)], 1, identity), outline = F)
    
    ### TESTING
    res_testing <- predictkde("testing", selected_bandwiths = selected_bandwiths_ic, selected_lambdas = selected_lambdas_ic)
    
    # all_crps <- getItem(res_testing$results, "crps")
    all_qf  <- getfromlist(res_testing$results, "q_hat")
    all_mf  <- getfromlist(res_testing$results, "mu_hat")
    all_varf  <- getfromlist(res_testing$results, "var_hat")
    
    save(file = res_file, list = c("all_qf", "all_mf", "all_varf"))
    
    ### IN SAMPLE INFO
    res_insample_info <- predictkde("insample_info", selected_bandwiths = selected_bandwiths_ic, selected_lambdas = selected_lambdas_ic)

    # residuals
    all_residuals <- getfromlist(res_insample_info$results, "residuals")
    e_residuals_unscaled <- unlist(all_residuals)
    all_var <- getfromlist(res_insample_info$results, "var_hat")
    all_mu <- getfromlist(res_insample_info$results, "mu_hat")
    
    all_varhat <- unlist(all_var)
    e_residuals <- e_residuals_unscaled/sqrt(all_varhat)
    
    # save residuals COPULA
    dir.create(file.path(insample.folder, algo), recursive = TRUE, showWarnings = FALSE)
    resid_file <- file.path(insample.folder, algo, paste("residuals_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = resid_file, list = c("e_residuals"))
    
    # save residuals MINT
    residuals_MINT <- e_residuals_unscaled
    resid_MINT_file <- file.path(insample.folder, algo, paste("residuals_MINT_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = resid_MINT_file, list = c("residuals_MINT"))
    
    # extract insample quantiles
    all_qf_insample  <- getfromlist(res_insample_info$results, "q_hat")

    all_qfe_insample <- lapply(seq_along(length(all_qf_insample)), function(iday){
      t((t(all_qf_insample[[iday]]) - all_mu[[iday]])/sqrt(all_var[[iday]]))
    })
    
    insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", idseries, "_", algo, ".Rdata", sep = "")) 
    save(file = insample_condmean_file, list = c("all_mu"))

    #insample_quantile_file <- file.path(insample.folder, algo, paste("quantiles_", idseries, "_", algo, ".Rdata", sep = "")) 
    #save(file = insample_quantile_file, 
    #     list = c(all_qfe_insample"))
    #list = c("all_qf_insample", "all_qfe_insample"))

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
        
        qf <- matrix(NA, nrow = length(taus), ncol = 48)
        for(h in seq(48))
          qf[,h] <- qnorm(taus, mu_hat[h], sd_hat[h])
          #qf[,h] <- qtnorm(taus, mean= mu_hat[h], sd= sd_hat[h], lower=0, upper=Inf, lower.tail = TRUE, log.p = FALSE)
        
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
