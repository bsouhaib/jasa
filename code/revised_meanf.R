rm(list = ls())
args = (commandArgs(TRUE))
if(length(args) == 0){
  do.agg <- F
  alliseries <- c(2)
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

do.shrink_towards_base <- FALSE

for(iseries in alliseries){
  
   # HERE !!!!!!
  load(file.path(work.folder, "revisedf", paste("revised_meanf_Xmatrix.Rdata", sep = "")))
  
  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  # icol <- match(idseries, allinputSeries)
  # 

  demand_idseries <- demand
  
  y_learn <- demand_idseries[learn$id]
  y_test <- demand_idseries[test$id]
  
  if(!do.agg && do.shrink_towards_base){
    icol <- match(idseries, allinputSeries)
    y_learn <- y_learn - Xhat_learn[, icol]
    #y_test <-  y_test - X_test[, icol]
  }
  
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
  
  #if(iseries == 12)
  #print(model_final)
  
  # testing
  mu_revised_alltest <- as.numeric(predict(model_final, X_test))
  
  if(!do.agg && do.shrink_towards_base){
    mu_revised_alltest <- mu_revised_alltest + X_test[, icol]
  }
  #stop("done")
  
  res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", idseries, ".Rdata", sep = "")) 
  save(file = res_file, list = c("mu_revised_alltest", "model_final"))

}

################
if(FALSE){
  
  plot.ts(head(y_test, 48*7*2))
  lines(X_test[, match(idseries, allinputSeries)], col = "red")
  
  err_new <- err_old <- NULL
  savepdf(file.path(results.folder, paste("PREDTEST_", nvar_additional, sep = "") ))
  for(ijob in seq(92)){
    index <- (ijob - 1) * 48 +  seq(48)
    new_pred <- mu_revised_alltest[index]
    actual <- demand_idseries[test$id[index]]
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
    
    res_byidtest_file <- file.path(work.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = "")) 
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
  err_mint <- NULL
  for(ijob in seq(92)){
    index <- (ijob - 1) * 48 +  seq(48)
    mintpred <- mintf[index]
    actual <- demand[test$id[index]]
    err_mint <- c(err_mint, (mintpred - actual)^2)
  }
  lines(apply(matrix(err_mint, ncol = 48, byrow = T), 2, mean), col = "purple")
}

#  model_final$beta[match(idseries, allinputSeries)]