#A <- function(mux, varx){
#  2 * sqrt(varx) * dnorm(mux / sqrt(varx)) + mux * (2 * pnorm(mux / sqrt(varx)) - 1)
#}

predictkde <- function(task = c("learning", "testing", "insample_info"), selected_bandwiths = NULL){

  n_past_obs <- n_past_obs_kd
  
  if(task == "learning"){
    
    #ids_past   <- train$id
    #ids_future <- validation$id
    
    ids_past   <- tail(train$id, n_past_obs)
    ids_future <- validation$id
    
    ##### Bandwith interval #####
    #n_base <- length(train$id)
    n_base <- n_past_obs
    if(grepl("KD-D", algo)){
      n_approx <- n_base/48
    }else if(grepl("KD-IC", algo)){
      #n_approx <- n_base/144 # 144 = 48 * 3
      n_approx <- (n_base/336)*4  #(should be 5 for weekdays and 3 for weekends. So (3+5)/2 = 4)
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
    #h_silver <- 0.9 * min(sd(x_samples), IQR(x_samples)/1.349) * n_approx ^(-.2)
    #if(h_silver == 0){
    #  h_silver <- 0.01
    #}
    #bandwiths <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 3, 4, 5) * h_silver	
    
    #bw_normal <- bw.nrd(sample(demand[ids_past], n_approx))
    #bandwiths <-  seq(10^-6, bw_normal, length.out = 10)
    
    
    res <- sapply(seq(10), function(l){
      sapply(seq(48), function(h){bw.nrd(sample(demand[ids_past][seq(h, length(ids_past), by = 48)], n_approx))})
    })
    bw_normal <- max(apply(res, 1, mean))
    
    min_bandwith <- 10^-5
    bandwiths_vec <- seq(min_bandwith, bw_normal, length.out = 5)
    bandwiths_subvec1 <- seq(bandwiths_vec[1], bandwiths_vec[2], length.out = 5)
    bandwiths_subvec2 <- seq(bandwiths_vec[2], bandwiths_vec[3], length.out = 5)
    bandwiths <- c(bandwiths_subvec1, bandwiths_subvec2, bandwiths_vec[-seq(3)])
    #bandwiths <-  seq(10^-4, bw_normal, length.out = 15)
    
    stopifnot(all(bandwiths>0))
    
    #bandwiths <- seq(1/1000, 1, by = 0.001) * h_silver
  
    nb_futuredays <- length(seq_validation_interval)/48
    
  }else if(task == "testing"){
    stopifnot(length(selected_bandwiths) == 3)
    #ids_past   <- learn$id
    #ids_future <- test$id
    
    ids_past   <- tail(learn$id, n_past_obs)
    ids_future <- test$id
    
    nb_futuredays <- length(seq_testing_interval)/48
  }else if(task == "insample_info"){
    stopifnot(length(selected_bandwiths) == 3)
    
    ids_past   <- head(learn$id, n_past_obs)
    ids_future <- tail(learn$id, -n_past_obs)
    nb_futuredays <- length(ids_future)/48
  }
  
  #res_nighthours <- res_dayhours <- vector("list", nb_futuredays)
  results <- vector("list", nb_futuredays)
  
  ic_days <- calendar$periodOfCycle[ids_future][seq(1, length(ids_future), by = 48)]
  #browser()
  #stopifnot(length(ic_days) == nb_futuredays)
  
  for(id_future_day in seq(1, nb_futuredays)){
    #print(id_future_day)
    
    offset_nhours <- (id_future_day - 1) * 48
    
    ids_future_hours <- ids_future[offset_nhours + seq(1, 48)] 
    

    if(offset_nhours > 0){
      #ids_past_actual <- c(tail(ids_past, -offset_nhours), head(ids_future, offset_nhours))
      ids_past_actual <- c(ids_past, ids_future)[offset_nhours + seq(n_past_obs)]
    }else{
      ids_past_actual <- ids_past
    }
   
    # if day is IC 1, 2, ou 3 use different bandwiths
    # mybandwith is eiter a vector or a number
    if(task == "testing" || task == "insample_info"){
      ic_day <-  ic_days[id_future_day]
      bandwiths <- selected_bandwiths[ic_day]
    }
    results[[id_future_day]] <- lapply(ids_future_hours, function(id){kde(id, ids_past_actual, bandwiths, task)})
    
    # 48-hours ahead forecasts
    #res_nighthours[[id_future_day]] <- lapply(ids_future_nighthours, function(id){kde(id, ids_past_actual, bandwiths_nighthours, task)})
    #res_dayhours[[id_future_day]] <- lapply(ids_future_dayhours, function(id){kde(id, ids_past_actual, bandwiths_dayhours, task)})	
  }	
  list(results = results, bandwiths = bandwiths, ic_days = ic_days)
  #list(res_nighthours = res_nighthours, res_dayhours = res_dayhours, 
  #     bandwiths_nighthours = bandwiths_nighthours, bandwiths_dayhours = bandwiths_dayhours)
}

kde <- function(id_query, ids_data, bandwiths, task){
  #print(id_query)
  ####
  if(algo == "KD-U"){
    ids_data_kept <- ids_data
  }else if(grepl("KD-D", algo)){
    ids_data_kept <- ids_data[which(calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query])]
  }else if(grepl("KD-IC", algo)){
    
    #ids_data_kept <- ids_data[which(calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] 
    #                                & calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query])  ]
  
    if(calendar$periodOfCycle[id_query] == 1){ # MONDAY TO FRIDAY
      mycond <- calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
        calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query]
    }else{ # SATURDAY AND SUNDAY
      if(calendar$periodOfDay[id_query] == 1){
        mycond <- (calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
                     calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(0, 1))) | 
          (calendar$dweek[ids_data] == (calendar$dweek[id_query] - 1) & calendar$periodOfDay[ids_data] == 48)
      }else if(calendar$periodOfDay[id_query] == 48){
        mycond <- (calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
                     calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(-1, 0))) | 
          (calendar$dweek[ids_data] == (calendar$dweek[id_query]%%7 + 1) & calendar$periodOfDay[ids_data] == 1)
      }else{
        mycond <- calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
          calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(-1, 1))
      }
    }
    ids_data_kept <- ids_data[which(mycond)]
  }
  # calendar$periodOfDay[ids_data] %in% calendar$periodOfDay[id_query]
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
    
    if(task != "insample_info"){
      if(mykernel == "normal"){
        crps[i] <- crps_mixture(x, vech, obs)
      }else{
        invkcdf <- approxfun(cdf, xgrid, rule = 2)
        X1 <- invkcdf(u1)
        crps[i] <- mean(abs(X1 - obs)) - 0.5 * mean(abs(X1 - invkcdf(u2)))
      }
    }

    
    
  }# bandwiths
  #browser()
  
  if(task == "learning"){
    ret <- crps #list(crps = crps)
  }else if(task == "testing"){
    ret <- list(crps = crps, squared_error = squared_error, qtauhat = xgrid, tauhat = cdf, mu_hat = mu_hat, var_hat = var_hat)
  }else if(task == "insample_info"){
    ret <- list(residuals = residuals, qtauhat = xgrid, tauhat = cdf, mu_hat = mu_hat, var_hat = var_hat, mu_hat = mu_hat)
  }else{
    stop("ERROR ...")
  }
  ret
}

mint_betastar <- function(W, y_hat){
  MAT1 <- W %*% U
  MAT2 <- crossprod(U,MAT1)
  MAT3 <- tcrossprod(solve(MAT2), U)
  C1 <- J %*% MAT1
  C2 <- MAT3 %*% y_hat
  adj <- C1 %*% C2
  -adj
}

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

kdeOLD <- function(id_query, ids_data, bandwiths, task){
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
  # calendar$periodOfDay[ids_data] %in% calendar$periodOfDay[id_query]
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
    
    if(task != "insample_info"){
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
  
  
  if(task == "learning"){
    ret <- list(crps = crps)
  }else if(task == "testing"){
      ret <- list(crps = crps, squared_error = squared_error, qtauhat = xgrid, tauhat = cdf, mu_hat = mu_hat, var_hat = var_hat)
  }else if(task == "insample_info"){
      ret <- list(residuals = residuals, qtauhat = xgrid, tauhat = cdf)
   }else{
      stop("ERROR ...")
  }
  ret
}

predictkdeOLD <- function(task = c("learning", "testing", "insample_info"), bandwiths_nighthours = NULL, bandwiths_dayhours = NULL){
  
  n_past_obs <- n_past_obs_kd
  
  if(task == "learning"){
    
    #ids_past   <- train$id
    #ids_future <- validation$id
    
    ids_past   <- tail(train$id, n_past_obs)
    ids_future <- validation$id
    
    ##### Bandwith interval #####
    #n_base <- length(train$id)
    n_base <- n_past_obs
    if(grepl("KD-D", algo)){
      n_approx <- n_base/48
    }else if(grepl("KD-IC", algo)){
      #n_approx <- n_base/144 # 144 = 48 * 3
      n_approx <- (n_base/336)*4  #(should be 5 for weekdays and 3 for weekends. So (3+5)/2 = 4)
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
  }else if(task == "insample_info"){
    ids_past   <- head(learn$id, n_past_obs)
    ids_future <- tail(learn$id, -n_past_obs)
    nb_futuredays <- length(ids_future)/48
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
    res_nighthours[[id_future_day]] <- lapply(ids_future_nighthours, function(id){kde(id, ids_past_actual, bandwiths_nighthours, task)})
    res_dayhours[[id_future_day]] <- lapply(ids_future_dayhours, function(id){kde(id, ids_past_actual, bandwiths_dayhours, task)})	
  }	
  
  list(res_nighthours = res_nighthours, res_dayhours = res_dayhours, 
       bandwiths_nighthours = bandwiths_nighthours, bandwiths_dayhours = bandwiths_dayhours)
}

getfromlist <- function(mylist, item = c("crps", "residuals", "squared_error", "qtauhat", "tauhat", "mu_hat", "var_hat")){
  lapply(mylist, function(daylist){
    sapply(daylist, function(hourlist){
      hourlist[[item]]
    }, simplify = "array")
  })
}


getItem <- function(mylist, item, order_hours){
    item_night <- getfromlist(mylist$res_nighthours, item)
    item_day   <- getfromlist(mylist$res_dayhours, item)
    if(length(dim(item_night)) == 3){
      item_all <- abind(item_night, item_day, along = 2)
      item_all <- item_all[, order_hours, ]
      res <- lapply(seq(dim(item_all)[3]), function(iday){
        item_all[, , iday]
      })
    }else if(length(dim(item_night)) == 2){
      item_all <- rbind(item_night, item_day)
      item_all <- item_all[order_hours, ]
      res <- lapply(seq(ncol(item_all)), function(iday){
        item_all[, iday]
      })
    }
    res
}