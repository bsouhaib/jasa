rm(list = ls())
args = (commandArgs(TRUE))
if(length(args) == 0){
  do.agg <- F
  alliseries <- c(918, 12)
}else{
  
  for(i in 1:length(args)){
    print(args[[i]])
  }
  
  do.agg <- as.logical(args[[1]])
  alliseries <- NULL
  for(i in seq(2, length(args))){
    alliseries <- c(alliseries, as.numeric(args[[i]]))
  }
}
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)
library(Matrix)
library(glmnet)

library(quantreg)
library(rqPen)

load(file.path(work.folder, "myinfo.Rdata"))

algo.agg <- "DYNREG"
algo.bottom <- "KD-IC-NML"

for(iseries in alliseries){
  
  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }

  demand_idseries <- demand
  
  use.all <- TRUE
  if(use.all){
    allinputSeries <- c(aggSeries, bottomSeries)
  }else{
    node <- V(itree)[match(idseries, names(V(itree)))]
    ancestors <- subcomponent(itree, node, mode = "in")[-1]
    siblings_nodes <- ego(itree, order = 1, nodes = ancestors[1], mode = "out")[[1]][-1]
    agg_nodes_needed <- names(ancestors)
    bottom_nodes_needed <- names(siblings_nodes)
    #allinputSeries <- c(agg_nodes_needed, bottom_nodes_needed)
    allinputSeries <- c(aggSeries, bottom_nodes_needed)
  }
  n_inputs <- length(allinputSeries)
  
  #nbpast <- 48 * 7 * 4 * 3 + length(validation$id)
  #idt <- tail(learn$id, nbpast)
  ids_past <- learn$id
  nbpast <- length(ids_past)
  
  nvar_additional <- 0
  Xhat_learn <- matrix(NA, nrow = nbpast, ncol = n_inputs + nvar_additional)
  y_learn <- demand_idseries[ids_past]
  
  X_test <- matrix(NA, nrow = length(test$id), ncol = n_inputs + nvar_additional)
  y_test <- demand_idseries[test$id]
  
    
  for(i_xvar in seq_along(allinputSeries)){
    
    xvar_series <- allinputSeries[i_xvar]
    #print(xvar_series)
    
    if(xvar_series %in% bottomSeries){
      algo <- algo.bottom
    }else if(xvar_series %in% aggSeries){
      algo <- algo.agg
    }
    
    if(algo == "KD-IC-NML"){
      insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", xvar_series, "_", algo, ".Rdata", sep = "")) 
    }else if(algo == "DYNREG"){
      insample_condmean_file <- file.path(insample.folder, algo, paste("condmean_", xvar_series, "_", algo, "_", 1, ".Rdata", sep = "")) 
    }
  
    #
    load(insample_condmean_file)
    if(algo == "KD-IC-NML"){
      all_mu <- unlist(all_mu)  
      all_mu <- c(rep(NA, n_past_obs_kd), all_mu)
    }
    all_mu 
    Xhat_learn[, i_xvar] <-  all_mu
    
    #
    res_file <- file.path(basef.folder, algo, paste("results_", xvar_series, "_", algo, ".Rdata", sep = "")) 
    load(res_file) # all_mf
    X_test[, i_xvar] <- unlist(all_mf)
  }
  
  if(nvar_additional != 0){
    Xhat_learn[, ncol(Xhat_learn)] <- as.factor(calendar$periodOfDay[learn$id])
    X_test[, ncol(X_test)] <- as.factor(calendar$periodOfDay[test$id])
    
    Xhat_learn[, ncol(Xhat_learn) - 1] <- as.factor(calendar$tyear[learn$id])
    X_test[, ncol(X_test) - 1] <- as.factor(calendar$tyear[test$id])
    
    #X_learn_fourier <- fourier.series(calendar$periodOfDay[learn$id], 2, 48)
    #X_test_fourier <- fourier.series(calendar$periodOfDay[test$id], 2, 48)
    #nvar <- ncol(Xhat_learn)
    #Xhat_learn[, seq(nvar - nvar_additional + 1, nvar)] <- X_learn_fourier
    #X_test[, seq(nvar - nvar_additional + 1, nvar)] <- X_test_fourier
  }
  
  #my_penalty <- rep(1, ncol(Xhat_learn))
  my_penalty <- c(rep(1, ncol(Xhat_learn) - nvar_additional), rep(0, nvar_additional))
  #my_penalty <- c(rep(1, ncol(Xhat_learn) - nvar_additional), 0, 1) # time of year can be penalized
  
  # TRY TO KEEP THE ACTUAL MEAN UNPENALIZED
  # vec <- rep(1, ncol(Xhat_learn) - nvar_additional)
  # vec[match(idseries, allinputSeries)] <- 0
  # my_penalty <- c(vec, rep(0, nvar_additional))
  
  # remove na
  ikeep <- which(complete.cases(Xhat_learn))
  Xhat_learn <- Xhat_learn[ikeep, ]
  y_learn <- y_learn[ikeep]
  
  nlearn <- nrow(Xhat_learn)
  itrain <- seq(round(0.75*nlearn)) 
  #set.seed(1986); 
  #itrain <- sample(nlearn, round(0.75*nlearn))
  ivalid <- setdiff(seq(nlearn), itrain)
  
  Xhat_train <- Xhat_learn[itrain, ]; y_train <- y_learn[itrain]

  Xhat_valid <- Xhat_learn[ivalid, ]; y_valid <- y_learn[ivalid]
  
  myalpha <- .95
  use.intercept <- TRUE
  
  
  model <- glmnet(y = y_train, x = Xhat_train, alpha = myalpha, penalty.factor = my_penalty, intercept = use.intercept)
  pred <- predict(model, Xhat_valid)
  val.err <- apply((pred - y_valid)^2, 2, mean)
  
  #plot.ts(y_valid)
  # lines(pred[, 100], col = "red")
  # lines(pred[, 1], col = "red")
  
  #stop("done")
  #plot.ts(val.err)
  #res <- cv.glmnet(y = y_learn, x = Xhat_learn, alpha = myalpha, nfolds = 3)
  
  best_lambda <- model$lambda[which.min(val.err)]
  model_final <- glmnet(y = y_learn, x = Xhat_learn, alpha = myalpha, lambda = best_lambda, penalty.factor = my_penalty, intercept = use.intercept)
  
  print(model_final)
  
  if(iseries == 12)
  stop("done")
  
  # testing
  mu_revised_alltest <- as.numeric(predict(model_final, X_test))
  res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", idseries, ".Rdata", sep = "")) 
  save(file = res_file, list = c("mu_revised_alltest", "model_final"))

}

if(FALSE){
  err_new <- err_old <- NULL
  savepdf(file.path(results.folder, paste("PREDTEST_", nvar_additional, sep = "") ))
  for(ijob in seq(92)){
    index <- (ijob - 1) * 48 +  seq(48)
    new_pred <- mu_revised_alltest[index]
    actual <- demand[test$id[index]]
    old_pred <- X_test[index, match(idseries, allinputSeries)]
  
    err_new <- c(err_new, (new_pred - actual)^2)
    err_old <- c(err_old, (old_pred - actual)^2)
    
    matplot(cbind(new_pred, actual, old_pred), type = 'l', col = c("blue", "black", "red"))
  }
  dev.off()
  
  X <- NULL
  X <- cbind(X, apply(matrix(err_new, ncol = 48, byrow = T), 2, mean))
  X <- cbind(X, apply(matrix(err_old, ncol = 48, byrow = T), 2, mean))
  matplot(X, type = 'l', lty = 1, col = c("blue", "red"))
  
  ####
  covmethod <- c("shrink")
  W1file <- file.path(work.folder, "wmatrices", paste("W1_", algo.agg, "_", algo.bottom, "_", covmethod, ".Rdata", sep = "")) 
  load(W1file)
  J <- Matrix(cbind(matrix(0, nrow = n_bottom, ncol = n_agg), diag(n_bottom)), sparse = TRUE)
  U <- Matrix(rbind(diag(n_agg), -t(Sagg)), sparse = TRUE)
  
  ntest <- length(test$id)
  ADJ <- B <- matrix(NA, nrow = ntest, ncol = n_bottom)
  
  for(idtest in seq(length(test$id))){
    if(idtest%%100 == 0)
    print(idtest)
    
    res_byidtest_file <- file.path(basef.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
    load(res_byidtest_file)
    b_hat <- mean_bottom_idtest
    a_hat <- mean_agg_idtest 
    y_hat <- c(a_hat, b_hat)
    adj_bottom_MINT <- mint_betastar(W1, y_hat = y_hat)
    #adj_agg_MINT    <- Sagg %*% adj_bottom_MINT
    adj_bottom_MINT <- as.numeric(adj_bottom_MINT)
    #adj_agg_MINT    <-  as.numeric(adj_agg_MINT)
    ADJ[idtest, ] <- adj_bottom_MINT
    B[idtest, ] <- b_hat
  }
}

#  model_final$beta[match(idseries, allinputSeries)]