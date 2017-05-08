#A <- function(mux, varx){
#  2 * sqrt(varx) * dnorm(mux / sqrt(varx)) + mux * (2 * pnorm(mux / sqrt(varx)) - 1)
#}

getInfoNode <- function(typeinfo)
{
  if(typeinfo == "nb_nodes"){
    info_nodes_agg <- apply(Sagg, 1, sum)
    info_nodes_bottom <- rep(1, n_bottom)
  }else if(typeinfo == "kwh"){
    for(do.agg in c(TRUE, FALSE)){
      nseries <- ifelse(do.agg, n_agg, n_bottom)
      x <- numeric(nseries)
      for(iseries in seq(nseries)){
        if(do.agg){
          idseries <- aggSeries[iseries]
          load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
        }else{
          idseries <- bottomSeries[iseries]
          load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
        }
        #x[iseries] <- mean(demand)
        x[iseries] <- mean(apply(matrix(demand, nrow = 2), 2, sum))
      }
      if(do.agg){
        info_nodes_agg <- x
      }else{
        info_nodes_bottom <- x
      }
    }
  }
  list(info_nodes_agg = info_nodes_agg, info_nodes_bottom = info_nodes_bottom)
}

iterate <- function(theta, y, e_0, l_0, d_0, w_0, do.forecast = FALSE){
  phi   <- theta[1]
  alpha <- theta[2]
  delta <- theta[3]
  omega <- theta[4]
  
  n <- length(y)
  yhat <- e <- l <- d <- w <- numeric(n) + NA
  if(do.forecast){
    H <- 48
    stopifnot(H <= 48)
    nf <- n + H   
    ysim <-  yhat <- e <- l <- d <- w <- numeric(nf) + NA
  }
  
  
  yhat_local_0 <- e_local_0 <- l_local_0 <- d_local_0 <- w_local_0 <- z <- numeric(m_2 * 2) + NA
  e_local_0[seq(1, m_2)] <- e_0
  l_local_0[seq(1, m_2)] <- l_0
  d_local_0[seq(1, m_2)] <- d_0
  w_local_0[seq(1, m_2)] <- w_0
  z[seq(m_2 + 1, 2 * m_2)] <- y[seq(m_2)]
  
  for(i in seq(m_2 + 1, 2 * m_2)){
    
    yhat_local_0[i]   <- l_local_0[i - 1] + d_local_0[i - m_1] + w_local_0[i - m_2] + phi * e_local_0[i - 1]
    e_local_0[i] <- z[i] - (l_local_0[i - 1] + d_local_0[i - m_1] + w_local_0[i - m_2]) # added - m_2 for y
    l_local_0[i] <- l_local_0[i - 1] + alpha * e_local_0[i]
    d_local_0[i] <- d_local_0[i - m_1] + delta * e_local_0[i]
    w_local_0[i] <- w_local_0[i - m_2] + omega * e_local_0[i]
  }
  yhat[seq(m_2)]   <- yhat_local_0[m_2 + seq(1, m_2)]
  e[seq(m_2)] <- e_local_0[m_2 + seq(1, m_2)]
  l[seq(m_2)] <- l_local_0[m_2 + seq(1, m_2)]
  d[seq(m_2)] <- d_local_0[m_2 + seq(1, m_2)]
  w[seq(m_2)] <- w_local_0[m_2 + seq(1, m_2)]
  
  for(i in seq(m_2 + 1, n)){
    yhat[i]   <- l[i - 1] + d[i - m_1] + w[i - m_2] + phi * e[i - 1]
    e[i] <- y[i] - (l[i - 1] + d[i - m_1] + w[i - m_2])
    l[i] <- l[i - 1] + alpha * e[i]
    d[i] <- d[i - m_1] + delta * e[i]
    w[i] <- w[i - m_2] + omega * e[i]
  }
  
  # mean forecast
  if(do.forecast){
    
    for(h in seq(H)){
      i <- n 
      stopifnot(h <= m_1)
      # yhat[i + h] <- l[i] + (alpha * phi * (1 - phi^(h - 1))) / ((1 - phi) * e[i])  + d[i - m_1 + h] + w[i - m_2 + h] + phi^h * e[i]
      yhat[i + h] <- l[i] + (alpha * phi * (1 - phi^(h - 1)) / (1 - phi)) * e[i]  + d[i - m_1 + h] + w[i - m_2 + h] + phi^h * e[i]
    }
    mf <- yhat[n + seq(H)]
  }
  
  # sample paths + quantile forecast
  if(do.forecast){
    K <- 5000
    samples <- matrix(NA, nrow = K, ncol = H)
    
    residuals <- y - head(yhat, -H)
    mat_residuals <- matrix(residuals, ncol = 48, byrow = T)
    # bootstrap vectors of size 48 from the residuals
    ind <- sample(seq(nrow(mat_residuals)), K, replace = TRUE)
    
    for(k in seq(K)){
      varepsilon <- mat_residuals[ind[k], ]
      for(h in seq(H)){
        i <- n + h
        ysim[i]   <- l[i - 1] + d[i - m_1] + w[i - m_2] + phi * e[i - 1] + varepsilon[h]
        e[i] <- ysim[i] - (l[i - 1] + d[i - m_1] + w[i - m_2])
        l[i] <- l[i - 1] + alpha * e[i]
        d[i] <- d[i - m_1] + delta * e[i]
        w[i] <- w[i - m_2] + omega * e[i]
      }
      samples[k, ] <- ysim[n + seq(H)]
    }
    qf <- apply(samples, 2, quantile, prob = taus)
    # matplot(t(qf), type = 'l', lty = 1)
    mfsample <- apply(samples, 2, mean)
    
    yhat <- head(yhat, -H)
    e    <- head(e, -H)
    l    <- head(l, -H)
    d    <- head(d, -H) 
    w    <- head(w, -H)
  }
  
  if(do.forecast){
    RET <- list(yhat = yhat, e = e, l = l, d = d, w = w, mf = mf, qf = qf, residuals = residuals, mfsample = mfsample)
  }else{
    RET <- list(yhat = yhat)
  }
  RET
}

func_to_optimize <- function(theta, y, e_0, l_0, d_0, w_0, do.forecast){
  obj <- iterate(theta, y, e_0, l_0, d_0, w_0, do.forecast)
  mean((y - obj$yhat)^2)
}


gtop <- function(ycheck, R, nagg, ntotal, Sagg, P_bu){
  Rinv <- solve(R) #Rinv <- diag(1 / sqrt(weights_GTOP)) 
  A <- R
  A2 <- t(A) %*% A # A2 = R^T %*% R 
  
  dvec <- A2 %*% ycheck
  Dmat <- Rinv
  Amat <- diag(ntotal)[seq(nagg), ] - Sagg %*% P_bu 
  meq <- nagg
  Amat <- t(Amat)
  res <- solve.QP(Dmat, dvec, Amat, bvec = rep(0, meq), meq = meq, factorized = TRUE)
  #print(res)
  yproj <- res$solution
}

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

predictkde <- function(task = c("learning", "testing", "insample_info"), selected_bandwiths = NULL, selected_lambdas = NULL){

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
    
    lambdas <- c(0.8, 0.9, 0.95, 0.99)
    
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
      lambdas <- selected_lambdas[ic_day]
    }
    
    #  results[[id_future_day]] <- lapply(ids_future_hours, function(id){kde(id, ids_past_actual, bandwiths, lambda, task)})
    if(length(lambdas) == 1){
      results[[id_future_day]] <-  lapply(ids_future_hours, function(id){kde(id, ids_past_actual, bandwiths, lambdas, task)})
    }else{
      results[[id_future_day]] <- lapply(lambdas, function(lambda){
      lapply(ids_future_hours, function(id){kde(id, ids_past_actual, bandwiths, lambda, task)})
     })
    }

    #browser()
    # 48-hours ahead forecasts
    #res_nighthours[[id_future_day]] <- lapply(ids_future_nighthours, function(id){kde(id, ids_past_actual, bandwiths_nighthours, task)})
    #res_dayhours[[id_future_day]] <- lapply(ids_future_dayhours, function(id){kde(id, ids_past_actual, bandwiths_dayhours, task)})	
  }	
  list(results = results, bandwiths = bandwiths, lambdas = lambdas, ic_days = ic_days)
  #list(res_nighthours = res_nighthours, res_dayhours = res_dayhours, 
  #     bandwiths_nighthours = bandwiths_nighthours, bandwiths_dayhours = bandwiths_dayhours)
}

kde <- function(id_query, ids_data, bandwiths, lambda, task){
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
      is_selected <- calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
        calendar$periodOfDay[ids_data] == calendar$periodOfDay[id_query]
    }else{ # SATURDAY AND SUNDAY
      if(calendar$periodOfDay[id_query] == 1){
        is_selected <- (calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
                     calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(0, 1))) | 
          (calendar$dweek[ids_data] == (calendar$dweek[id_query] - 1) & calendar$periodOfDay[ids_data] == 48)
      }else if(calendar$periodOfDay[id_query] == 48){
        is_selected <- (calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
                     calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(-1, 0))) | 
          (calendar$dweek[ids_data] == (calendar$dweek[id_query]%%7 + 1) & calendar$periodOfDay[ids_data] == 1)
      }else{
        is_selected <- calendar$periodOfCycle[ids_data] == calendar$periodOfCycle[id_query] & 
          calendar$periodOfDay[ids_data] %in% (calendar$periodOfDay[id_query] + seq(-1, 1))
      }
    }
    
    ids_data_kept <- ids_data[which(is_selected)]
    n <- length(ids_data_kept)
    normalized_weights <- rep(1, n)/n
    
    do.weighting <- TRUE
    if(do.weighting){
     weights_all <- lambda^floor((tail(ids_data) - ids_data)/336)
     weights_selected <- weights_all[which(is_selected)]
     normalized_weights <- weights_selected/sum(weights_selected)
    }
    #browser()
  }
  # calendar$periodOfDay[ids_data] %in% calendar$periodOfDay[id_query]
  # print(length(ids_data_kept))
  
  x <- demand[ids_data_kept]
  ####
  
  n <- length(x)
  minx <- min(x)
  maxx <- max(x)
  #logx <- log(x)
  
  #xgrid <- c(seq(from = minx , to = quantile(x, .9), length = 90), seq(from = quantile(x, .91), to = maxx  , length = 10))
  xgrid <- c(0, 
             seq(from = minx , to = quantile(x, .9), length = 90), 
             seq(from = quantile(x, .91), to = maxx, length = 10),
             seq(from = maxx, to = maxx + 3* max(bandwiths), length = 3))
  xgrid <- sort(unique(xgrid))
  
  crps <- residuals <- squared_error <- mu_hat <- var_hat <- numeric(length(bandwiths))
  for(i in seq_along(bandwiths)){
    h <- bandwiths[i]
    vech <- rep(h, length(x))
    
    if(mykernel == "normal"){
      #xgrid <- c(seq(from = 0 , to = quantile(x, .9), length = 90), seq(from = quantile(x, .91), to = maxx + 3*h  , length = 10))
      r <- 3
      ids_boundary <- which(x <= (minx + r * h))
      #vech[ids_boundary] <- pmax((x[ids_boundary] - minx) / r, pmin(10^-3, vech[ids_boundary]))
      vech[ids_boundary] <- pmax((x[ids_boundary] - minx) / r, 10^-3)
    }
    
    #else if(mykernel == "truncated"){
    #  lowerx <- minx
    #  upperx <- maxx * 1.5
    # q90 <- quantile(x, .9)
    # q91 <- quantile(x, .91)
    #  xgrid <- c(seq(from = lowerx , to = q90, length = 90), seq(from = q91, to = upperx  , length = 10))
    #}
    
    cdfs <- sapply(seq(length(x)), function(i){ 	
      xi <- x[i]		
      if(mykernel == "normal"){
        #pnorm((xgrid - obs)/h)/(n)
        #pnorm((xgrid - xi)/vech[i])/(n)
        pnorm((xgrid - xi)/vech[i]) * normalized_weights[i]
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
    #mu_hat[i] <- sum(all_mus)/n
    mu_hat[i] <- sum(all_mus * normalized_weights)
    
    if(mykernel == "normal"){
      #var_hat[i] <- sum(x^2)/n + sum(vech^2)/n - (mu_hat[i])^2
      var_hat[i] <- sum(normalized_weights * ((x - mu_hat[i])^2 + vech^2))
    }
    
    obs <- demand[id_query]
    
    residuals[i] <- obs - mu_hat[i] 
    squared_error[i] <- (residuals[i])^2 
    
    if(task != "insample_info"){
      if(mykernel == "normal"){
        crps[i] <- crps_mixture(x, vech, normalized_weights, obs)
      }else{
        invkcdf <- approxfun(cdf, xgrid, rule = 2)
        X1 <- invkcdf(runif(1000))
        crps[i] <- crps_sampling(X1, obs)
        #crps[i] <- mean(abs(X1 - obs)) - 0.5 * mean(abs(X1 - invkcdf(u2)))
      }
    }

    if(task != "learning"){
      invcdf <- approxfun(cdf, xgrid, rule = 2)
      q_hat <- invcdf(taus)
    }

  }# bandwiths
  
  if(task == "learning"){
    ret <- crps #list(crps = crps)
  }else if(task == "testing"){
    ret <- list(crps = crps, squared_error = squared_error, q_hat = q_hat, mu_hat = mu_hat, var_hat = var_hat)
  }else if(task == "insample_info"){
    ret <- list(residuals = residuals, q_hat = q_hat, mu_hat = mu_hat, var_hat = var_hat)
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

mint_pmatrix <- function(W){
  MAT1 <- W %*% U
  MAT2 <- crossprod(U,MAT1)
  MAT3 <- tcrossprod(solve(MAT2), U)
  C1 <- J %*% MAT1
  J - C1 %*% MAT3
}

hasnoBottom <- function(algo){
  grepl("BU", algo) & !grepl("NNLS", algo) 
}

getInfo <- function(idtest){
  iday <- floor((idtest-1)/48) + 1
  hour <- (idtest -1)%%48 + 1
  list(iday = iday, hour = hour)
}

crps_mixture <- function(mus, vars, weights, x_query){
  M <- length(mus)
  
  sigmas <- sqrt(vars); x_centered <- (x_query - mus)
  
  #comp1 <- sum(2 * sigmas * dnorm(x_centered / sigmas) + x_centered * (2 * pnorm(x_centered / sigmas) - 1))/M
  comp1_part1 <- 2 * sigmas * dnorm(x_centered / sigmas) + x_centered * (2 * pnorm(x_centered / sigmas) - 1)
  comp1 <- sum(weights * comp1_part1)
  
  ids <- permutations(n = M, r = 2, repeats.allowed=T)
  
  mudiffs <- mus[ids[, 1]] - mus[ids[, 2]]
  varsums <- vars[ids[, 1]] + vars[ids[, 2]]
  wproducts <- weights[ids[, 1]] * weights[ids[, 2]]
  
  sigmasums <- sqrt(varsums)
  comp2_part1 <- 2 * sigmasums * dnorm(mudiffs / sigmasums) + mudiffs * (2 * pnorm(mudiffs / sigmasums) - 1)
  comp2 <- sum(wproducts * comp2_part1)
  
  #comp2 <- (1/M^2) * sum(2 * sigmasums * dnorm(mudiffs / sigmasums) + 
  #                         mudiffs * (2 * pnorm(mudiffs / sigmasums) - 1))

  comp1 - 0.5 * comp2
}

crps_sampling <- function(X, obs){
  X <- sample(X)
  Xprime <- diff(X)
  mean(abs(X - obs)) - 0.5 * mean(abs(Xprime))
}



getfromlist <- function(mylist, item = c("crps", "residuals", "squared_error", "q_hat", "tauhat", "mu_hat", "var_hat")){
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