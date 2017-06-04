source("jasa_utils_ets.R")
source("jasa_utils_kde.R")
library(scoringRules)

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