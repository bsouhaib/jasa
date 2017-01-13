rm(list = ls())
set.seed(1986)
args = (commandArgs(TRUE))
if(length(args) == 0){
  #algo <- c("KD-IC-NML")
  algo <- "TBATS"
  do.agg <- TRUE
  ijob <- NA
  #alliseries <- NULL
  
}else{
  
  for(i in 1:length(args)){
    print(args[[i]])
  }
  
  algo <- args[[1]]
  do.agg <- as.logical(args[[2]])
  ijob <- as.numeric(args[[3]])
  
  #alliseries <- NULL
  #for(i in seq(3, length(args))){
  #  alliseries <- c(alliseries, as.numeric(args[[i]]))
  #}
  
}
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

#######
set_series <- bottomSeries
if(do.agg)
  set_series <- aggSeries

n <- length(set_series)

if(!do.agg){
  stopifnot(ijob <= 4)
  nb_alliseries <- ceiling(n/4)
  alliseries <- (ijob - 1) * nb_alliseries + seq(nb_alliseries)
  id <- which(alliseries > n)
  if(length(id) > 0){
    alliseries <- alliseries[-id]
  }
}else{
  alliseries <- seq(n_agg)
}
print(alliseries)
#######


#do.agg <- FALSE
#alliseries <- length(bottomSeries)
#alliseries <- seq(10)
#algo <- "KD-IC-NML"

nseries <- length(alliseries)
ntest <- length(test$id)

crps <- qcrps <- mse <- matrix(NA, nrow = nseries, ncol = ntest)

if(do.agg){
  rownames(qcrps) <- rownames(mse) <- aggSeries[alliseries]
}else{
  rownames(qcrps) <- rownames(mse) <- bottomSeries[alliseries]
}

for(i in seq_along(alliseries)){
  #print(date())
  print(i)
  
  if(do.agg){
    qs_all <- array(NA, c(M, ntest))
  }
  
  iseries <- alliseries[i]
  #print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
  load(res_file)
  
  for(idtest in seq(ntest)){
    
    iday <- getInfo(idtest)$iday
    hour <- getInfo(idtest)$hour
    
    if(algo == "Uncond" || algo == "PeriodOfDay"){
      
      invcdf <- approxfun(alphas, qFtest[, idtest], rule = 2)
      
    }else if(algo == "TBATS"){	
      
      invcdf <- approxfun(alphas, all_qf[[iday]][, hour], rule = 2)
      
    }else if(algo == "KD-IC-NML"){	
      
      if(hour %in% hours_night){
        index <- match(hour, hours_night)
        qtauhat <- res_testing$res_nighthours[[iday]][[index]]$qtauhat
        tauhat <- res_testing$res_nighthours[[iday]][[index]]$tauhat
      }else{
        index <- match(hour, hours_day)
        qtauhat <- res_testing$res_dayhours[[iday]][[index]]$qtauhat
        tauhat <- res_testing$res_dayhours[[iday]][[index]]$tauhat
      }
      invcdf <- approxfun(tauhat, qtauhat, rule = 2)
    }else{
      stop("error")
    }
    
    # random_sample <- invcdf(runif(M))
    ordered_random_sample <- invcdf(q_probs)
    
    obs <- demand[test$id[(iday - 1)*48 + hour]]
    
    #crps[i, idtest] <- crps_sampling(X = sample(ordered_random_sample), obs = obs)
    mse[i, idtest] <- (mean(ordered_random_sample) - obs)^2
    
    
    #plot(qtauhat, tauhat)
    qs <- sapply(seq(length(q_probs)), function(iprob){
      2 * ((obs <= ordered_random_sample[iprob]) - q_probs[iprob]) * (ordered_random_sample[iprob] - obs)
    })
    qcrps[i, idtest] <- sum(qs)/length(q_probs)
    
    if(do.agg){
      qs_all[, idtest] <- qs
    }

    #cdf <- function(x){approxfun(qtauhat, tauhat, rule = 2)(x)}
    #integrand <- function(x){
    #  (cdf(x) - Heaviside(x, a = obs))^2
    #}
    #lower_integration <- min(qtauhat)
    #upper_integration <- max(qtauhat)
    #crps_num_integration <- integrate(integrand, lower_integration, upper_integration)
    
    #stop("done")
    
    #if(no.bottom){
    # qs[, i, idtest] <- sapply(alphas, function(alpha){
    #    check_function(obs, quantile(random_sample, alpha), alpha)
    #  })
    #}
    
  }# idtest
  
  if(do.agg){
    dir.create(file.path(loss.folder, algo), showWarnings = FALSE)
    qs_file <- file.path(loss.folder, algo, paste("qs_", algo, "_", idseries, ".Rdata", sep = "")) 
    save(file = qs_file, list = c("qs_all"))
    
    res_mse_file <- file.path(loss.folder, algo, paste("mse_", algo, "_", idseries, ".Rdata", sep = "")) 
    save(file = res_mse_file, list = c("mse"))
  }
  
}# alliseries

if(!do.agg){

  dir.create(file.path(loss.folder, algo), showWarnings = FALSE)
  
  res_crps_file <- file.path(loss.folder, algo, paste("crps_", algo, "_", ijob, ".Rdata", sep = "")) 
  save(file = res_crps_file, list = c("qcrps"))
  
  res_mse_file <- file.path(loss.folder, algo, paste("mse_", algo, "_", ijob, ".Rdata", sep = "")) 
  save(file = res_mse_file, list = c("mse"))
}