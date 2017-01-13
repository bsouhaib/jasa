#A <- function(mux, varx){
#  2 * sqrt(varx) * dnorm(mux / sqrt(varx)) + mux * (2 * pnorm(mux / sqrt(varx)) - 1)
#}

hasnoBottom <- function(algo){
  grepl("BU", algo) & !grepl("NNLS", algo) 
}

getInfo <- function(idtest){
  iday <- floor((idtest-1)/48) + 1
  hour <- (idtest -1)%%48 + 1
  list(iday = iday, hour = hour)
}

crps_mixture <- function(mus, vars, x_query){
  M <- length(mus)
  
  # comp1 <- sum(A(x_query - mus, vars))/M
  sigmas <- sqrt(vars); x_centered <- (x_query - mus);
  comp1 <- sum(2 * sigmas * dnorm(x_centered / sigmas) +
                 x_centered * (2 * pnorm(x_centered / sigmas) - 1))/M
  
  ids <- permutations(n = M, r = 2, repeats.allowed=T)
  
  mudiffs <- mus[ids[, 1]] - mus[ids[, 2]]
  varsums <- vars[ids[, 1]] + vars[ids[, 2]]
  
  # comp2 <- (1/M^2) * sum(A(mudiffs, varsums))
  sigmasums <- sqrt(varsums); 
  comp2 <- (1/M^2) * sum(2 * sigmasums * dnorm(mudiffs / sigmasums) + 
                           mudiffs * (2 * pnorm(mudiffs / sigmasums) - 1))
  comp1 - 0.5 * comp2
}

crps_sampling <- function(X, obs){
  X <- sample(X)
  Xprime <- diff(X)
  mean(abs(X - obs)) - 0.5 * mean(abs(Xprime))
}

kde <- function(id_query, ids_data, bandwiths, only.mean = FALSE){
  #print(id_query)
  ####
  if(algo == "KD-U"){
    ids_data_kept <- ids_data
  }else if(grepl("KD-D", algo)){
    ids_data_kept <- ids_data[which(calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query])]
  }else if(grepl("KD-IC", algo)){
    ids_data_kept <- ids_data[which(calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] 
                                    & calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query])  ]
  }
  #print(length(ids_data_kept))
  
  x <- demand[ids_data_kept]
  ####
  
  n <- length(x)
  minx <- min(x)
  maxx <- max(x)
  #logx <- log(x)
  
  xgrid <- c(seq(from = minx , to = quantile(x, .9), length = 90), seq(from = quantile(x, .91), to = maxx  , length = 10))
  
  #pdf(file.path(results.folder, paste("cdfs", id, ".pdf" , sep = ""))) # REMOVE !!!!
  
  q90 <- quantile(x, .9)
  q91 <- quantile(x, .91)
  
  crps <- residuals <- squared_error <- mu_hat <- var_hat <- numeric(length(bandwiths))
  for(i in seq_along(bandwiths)){
    h <- bandwiths[i]
    vech <- rep(h, length(x))
    
    if(mykernel == "normal"){
      xgrid <- c(seq(from = 0 , to = quantile(x, .9), length = 90), seq(from = quantile(x, .91), to = maxx + 3*h  , length = 10))
      
      r <- 3
      ids_boundary <- which(x <= (minx + r * h))
      #vech[ids_boundary] <- pmax((x[ids_boundary] - minx) / r, pmin(10^-3, vech[ids_boundary]))
      vech[ids_boundary] <- pmax((x[ids_boundary] - minx) / r, 10^-3)
      
    }else if(mykernel == "truncated"){
      lowerx <- minx
      #upperx <- maxx + 5*h
      upperx <- maxx * 1.5
      xgrid <- c(seq(from = lowerx , to = q90, length = 90), seq(from = q91, to = upperx  , length = 10))
    }
    
    #########
    if(FALSE){
      bumps <- sapply(seq(length(x)), function(i){ 
        xi <- x[i]		
        if(mykernel == "normal"){
          #gauss((xgrid - obs)/h)/(n * h) 
          dnorm((xgrid - xi)/vech[i])/(n * vech[i]) 
        }else if(mykernel == "lognormal"){
          dlnorm(xgrid, meanlog = log(xi), sdlog = vech[i], log = FALSE)/n
        }else if(mykernel == "truncated"){
          dtnorm(xgrid, mean = xi, sd = vech[i], lower = lowerx, upper = upperx)/n
        }
      })
      mypdf <- rowSums(bumps)
      # apply(bumps, 2, function(b){ matplot(xgrid, bumps, lty = 1, type = 'n'); lines(xgrid, b)})
    }
    #########
    if(!only.mean){
      cdfs <- sapply(seq(length(x)), function(i){ 	
        xi <- x[i]		
        if(mykernel == "normal"){
          #pnorm((xgrid - obs)/h)/(n)
          pnorm((xgrid - xi)/vech[i])/(n)
        }else if(mykernel == "lognormal"){
          plnorm(xgrid, meanlog = log(xi), sdlog = vech[i], lower.tail = TRUE, log.p = FALSE)/n
        }else if(mykernel == "truncated"){
          ptnorm(xgrid, mean = xi, sd = vech[i], lower = lowerx, upper = upperx, lower.tail = TRUE, log.p = FALSE)/n
        }
      })
      cdf <- rowSums(cdfs)
    }
    #browser()
    
    # Mean forecasts 
    if(mykernel == "normal"){
      all_mus <- x
    }else if(mykernel == "lognormal"){
      all_mus <- sapply(x, function(xi){ exp(log(xi) + (h^2)/2) })
    }else if(mykernel == "truncated"){
      all_mus <- sapply(x, function(xi){ 	
        alpha <- (lowerx - xi)/h
        beta  <- (upperx - xi)/h
        xi + ((dnorm(alpha) - dnorm(beta))*h) / (pnorm(beta) - pnorm(alpha))	
      })
    }
    mu_hat[i] <- sum(all_mus)/n
    
    if(mykernel == "normal"){
      var_hat[i] <- sum(x^2)/n + sum(vech^2)/n - (mu_hat[i])^2
    }
    
    obs <- demand[id_query]
    
    residuals[i] <- obs - mu_hat[i] 
    squared_error[i] <- (residuals[i])^2 
    
    #modelcdf <- smooth.spline(xgrid, cdf)
    #kcdf <- function(x){
    #	predict(modelcdf, x)$y
    #}
    
    #kpdf <- approxfun(xgrid,mypdf)
    #integrand <- function(x){
    #	kpdf(x)
    #}
    #for(i in seq(2, length(xgrid))){
    #	print(integrate(integrand, min(xgrid), xgrid[i])$value)
    #	print(kcdf(xgrid[i]))
    #	print("---")
    #}
    
    #kcdf <- approxfun(xgrid,cdf)
    #integrand <- function(x){
    #	(kcdf(x) - Heaviside(x, a = obs))^2
    #}
    #crps[i] <- integrate(integrand, min(xgrid), max(xgrid))$value
    
    if(!only.mean){
      if(mykernel == "normal"){
        crps[i] <- crps_mixture(x, vech, obs)
      }else{
        invkcdf <- approxfun(cdf, xgrid, rule = 2)
        X1 <- invkcdf(u1)
        crps[i] <- mean(abs(X1 - obs)) - 0.5 * mean(abs(X1 - invkcdf(u2)))
      }
    }
    
     #if(FALSE){
     # check_function <- function(y, f, tau){
     #    tau*(y - f)*((y - f) >= 0) - (1-tau)*(y - f)*((y - f) < 0)
     #}
      
     # invkcdf <- approxfun(cdf, xgrid)
     #  u <- runif(10000)
     # qf <- invkcdf(u)
     #  allqloss <- sapply(seq(length(u)), function(i){
     #   check_function(obs, qf[i], u[i])
     # })
     # (2* sum(allqloss, na.rm = T))/10000
     #}
    
  }
  
  
  #pdf(paste("hour", id_query, ".pdf", sep = ""))
  #hist(x)
  #plot(bandwiths, crps)
  #abline(v = bandwiths[which.min(crps)], col = "red")
  #abline(v = bandwiths[21], col = "blue")
  #dev.off()
  #stop("done")
  #browser()
  
  
  if(length(bandwiths) == 1){ # TESTING
    if(!only.mean){
      ret <- list(crps = crps, squared_error = squared_error, qtauhat = xgrid, tauhat = cdf, mu_hat = mu_hat, var_hat = var_hat)
    }else{
      ret <- list(residuals = residuals, squared_error = squared_error, mu_hat = mu_hat, var_hat = var_hat)
    }
  }else{ # LEARNING
    ret <- list(crps = crps, squared_error = squared_error, qtauhat = NULL, tauhat = NULL, mu_hat = mu_hat, var_hat = var_hat)
  }
  
}

predictkde <- function(task = c("learning", "testing", "residuals"), bandwiths_nighthours = NULL, bandwiths_dayhours = NULL){
  
  n_past_obs <- n_past_obs_kd
  onlymean <- FALSE
  if(task == "learning"){
    
    #ids_past   <- train$id
    #ids_future <- validation$id
    
    ids_past   <- tail(train$id, n_past_obs)
    ids_future <- validation$id
    
   
    
    ##### Bandwith interval #####
    n_base <- length(train$id)
    if(grepl("KD-D", algo)){
      n_approx <- n_base/48
    }else if(grepl("KD-IC", algo)){
      n_approx <- n_base/144 # should not be 144
    }else{
      stop("error in algo")
    }
    stopifnot(!is.null(n_approx))
    
    #print(mykernel)
    #print("ok")
    
    
    if(mykernel == "normal" || mykernel == "truncated"){
      x_samples <- sample(demand[ids_past], n_approx)
    }else if(mykernel == "lognormal"){
      x_samples <- log(sample(demand[ids_past], n_approx))
    }
    
    #print("ok")
    h_silver <- 0.9 * min(sd(x_samples), IQR(x_samples)/1.349) * n_approx ^(-.2)
    
    if(h_silver == 0){
      h_silver <- 0.01
    }
    bandwiths <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 3, 4, 5) * h_silver	
    
   
    
    stopifnot(all(bandwiths>0))
    
    #bandwiths <- seq(1/1000, 1, by = 0.001) * h_silver
    
    bandwiths_nighthours <- bandwiths
    bandwiths_dayhours   <- bandwiths
    
    ##############################
    
    nb_futuredays <- length(seq_validation_interval)/48
    
  }else if(task == "testing"){
    #ids_past   <- learn$id
    #ids_future <- test$id
    
    ids_past   <- tail(learn$id, n_past_obs)
    ids_future <- test$id
    
    nb_futuredays <- length(seq_testing_interval)/48
  }else if(task == "residuals"){
    ids_past   <- head(learn$id, n_past_obs)
    ids_future <- tail(learn$id, -n_past_obs)
    
    nb_futuredays <- length(ids_future)/48
    
    onlymean <- TRUE
    #browser()
  }
  
  res_nighthours <- res_dayhours <- vector("list", nb_futuredays)
  
  for(id_future_day in seq(1, nb_futuredays)){
    #print(id_future_day)
    
    offset_nhours <- (id_future_day - 1) * 48
    
    #ids_future_hours <- ids_future[1 + offset_nhours] + seq(0, 47)
    ids_future_hours <- ids_future[offset_nhours + seq(1, 48)] 
    
    ids_future_nighthours <- ids_future_hours[which(calendar$periodOfDay[ids_future_hours] %in% hours_night)]
    ids_future_dayhours   <- ids_future_hours[which(calendar$periodOfDay[ids_future_hours] %in% hours_day)]
    
    if(offset_nhours > 0){
      #ids_past_actual <- c(tail(ids_past, -offset_nhours), head(ids_future, offset_nhours))
      ids_past_actual <- c(ids_past, ids_future)[offset_nhours + seq(n_past_obs)]
    }else{
      ids_past_actual <- ids_past
    }
    
    # print(length(ids_past_actual))
    # if(length(ids_past_actual) > 4080){
    #  browser()
    # }
     
    
    # 48-hours ahead forecasts
    res_nighthours[[id_future_day]] <- lapply(ids_future_nighthours, function(id){kde(id, ids_past_actual, bandwiths_nighthours, onlymean)})
    res_dayhours[[id_future_day]] <- lapply(ids_future_dayhours, function(id){kde(id, ids_past_actual, bandwiths_dayhours, onlymean)})	
  }	
  
  list(res_nighthours = res_nighthours, res_dayhours = res_dayhours, 
       bandwiths_nighthours = bandwiths_nighthours, bandwiths_dayhours = bandwiths_dayhours)
}

getfromlist <- function(mylist, item = c("crps", "squared_error", "qtauhat", "tauhat", "mu_hat")){
  sapply(mylist, function(daylist){
    sapply(daylist, function(hourlist){
      hourlist[[item]]
    }, simplify = "array")
  }, simplify = "array")
}