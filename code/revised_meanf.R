rm(list = ls())
set.seed(1987)
args = (commandArgs(TRUE))
if(length(args) == 0){
  do.agg <- T
  alliseries <- c(1) #  c(1406) 
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

#library(quantreg)
#library(rqPen)

load(file.path(work.folder, "myinfo.Rdata"))

#library(doParallel)
#registerDoParallel(4)

myalpha <- 1
nfolds <- 5
do.shrink_towards_base <- TRUE
include.calendar <- TRUE
use.intercept <- TRUE

for(iseries in alliseries){
  
  # HERE !!!!!!
  matrix_file <- file.path(work.folder, "revisedf", paste("revised_meanf_Xmatrix_", algo.agg, "_", algo.bottom, ".Rdata", sep = "")) 
  load(matrix_file)
  
  allvar <- colnames(Xhat_learn)
  stopifnot(all(allvar == colnames(Xhat_test)))
  
  print(iseries)
  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  
  
  demand_idseries <- demand
  
  y_learn <- demand_idseries[learn$id]
  y_test <- demand_idseries[test$id]
  
  if(!do.agg && do.shrink_towards_base){
    col_iseries <- match(idseries, allvar)
    y_learn <- y_learn - Xhat_learn[, col_iseries]
  }
  
  # remove na
  ikeep <- which(complete.cases(Xhat_learn))
  Xhat_learn <- Xhat_learn[ikeep, ]
  y_learn <- y_learn[ikeep]
  pday_learn <- pday_learn[ikeep]
  
  nlearn <- nrow(Xhat_learn)
  itrain <- seq(round(0.75*nlearn)) 
  ivalid <- setdiff(seq(nlearn), itrain)
  Xhat_train <- Xhat_learn[itrain, ]; y_train <- y_learn[itrain]
  Xhat_valid <- Xhat_learn[ivalid, ]; y_valid <- y_learn[ivalid]
  
  n_test <- nrow(Xhat_test)
  mu_revised_alltest_byh <- numeric(n_test)
  for(h in seq(48)){
    #print(h)
    
    #h_set <- h
    
    #h_set <- c(h - 1, h)
    h_set <- c(h - 1, h, h +1)
    id_problem <- which(h_set > 48 || h_set < 1)
    if(length(id_problem) > 0){
      h_set <- h_set[-id_problem]
    }
    #print(h_set)
    
    #idh_train <- which(Xhat_train[, 1634] %in% h_set)
    #y_train_h <- y_train[idh_train]
    #Xhat_train_h <- Xhat_train[idh_train, -1634]
    #idh_valid <- which(Xhat_valid[, 1634] %in% h_set)
    #Xhat_valid_h <- Xhat_valid[idh_valid, -1634]
    #y_valid_h <- y_valid[idh_valid]
    
    idh_learn <- which(pday_learn %in% h_set)
    Xhat_learn_h <- Xhat_learn[idh_learn, ]
    y_learn_h <- y_learn[idh_learn]
    
    idh_test <- which(pday_test == h)
    Xhat_test_h <- Xhat_test[idh_test, ]
    y_test_h <- y_test[idh_test]
    
    # HOLDOUT
    #model <- glmnet(y = y_train_h, x = Xhat_train_h, alpha = myalpha, penalty.factor = my_penalty, intercept = use.intercept)
    #pred <- predict(model, Xhat_valid_h)
    #val.err <- apply((pred - y_valid_h)^2, 2, mean)
    #best_lambda <- model$lambda[which.min(val.err)]
    
    # CROSS-VALIDATION
    n <- nrow(Xhat_learn_h)
    foldid <- rep(seq(nfolds), each = ceiling(n/nfolds))
    foldid <- head(foldid, n)
    #res <- cv.glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, 
    #                 penalty.factor = my_penalty, intercept = use.intercept, nfolds = nfolds, foldid = foldid,
    #                 parallel=TRUE, dfmax = 5)
    
    if(do.agg){
      model_seqlambda <- glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, intercept = use.intercept)
    }else{
      model_seqlambda <- glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, intercept = use.intercept, dfmax = 5)
    }
    res <- cv.glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, 
                     intercept = use.intercept,  foldid = foldid, lambda = model_seqlambda$lambda)
    
    #res <- cv.glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, 
    #                 penalty.factor = my_penalty, intercept = use.intercept, nfolds = nfolds,
    #                 parallel=TRUE, dfmax = 5)
    
    best_lambda <-  res$lambda.1se
    
    model_final <- glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, 
                          lambda = best_lambda, intercept = use.intercept)
    
    mu_revised_alltest_byh[idh_test] <- as.numeric(predict(model_final, Xhat_test_h))
    
    #beta <- as.numeric(model_final$beta)
    #id <- which(beta != 0)
    #print("---")
    #print(id)
    #print("---")
    
    if(!do.agg && do.shrink_towards_base){
      mu_revised_alltest_byh[idh_test] <- mu_revised_alltest_byh[idh_test] + Xhat_test_h[, col_iseries]
    }
    
  }
  
  mu_revised_alltest <- mu_revised_alltest_byh
  res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", idseries, ".Rdata", sep = "")) 
  save(file = res_file, list = c("mu_revised_alltest"))
  
  ### OLD CODE
  if(FALSE){
    model_total <- glmnet(y = y_train, x = Xhat_train, alpha = myalpha, intercept = use.intercept)
    pred_total <- predict(model_total, Xhat_valid)
    val.err <- apply((pred_total - y_valid)^2, 2, mean)
    best_lambda <- model_total$lambda[which.min(val.err)]
    model_final <- glmnet(y = y_learn, x = Xhat_learn, alpha = myalpha, 
                          lambda = best_lambda, intercept = use.intercept)
    
    # testing
    mu_revised_alltest <- as.numeric(predict(model_final, Xhat_test))
    
    if(!do.agg && do.shrink_towards_base){
      mu_revised_alltest <- mu_revised_alltest + Xhat_test[, col_iseries]
    }
    
    res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", idseries, ".Rdata", sep = "")) 
    save(file = res_file, list = c("mu_revised_alltest", "model_final"))
  }
  ###
}

################
if(FALSE){
  
  plot.ts(head(y_test, 48*7*2))
  lines(Xhat_test[, match(idseries, allinputSeries)], col = "red")
  
  err_new <- err_old <- NULL
  savepdf(file.path(results.folder, paste("PREDTEST_", nvar_additional, sep = "") ))
  for(ijob in seq(92)){
    index <- (ijob - 1) * 48 +  seq(48)
    new_pred <- mu_revised_alltest[index]
    actual <- demand_idseries[test$id[index]]
    old_pred <- Xhat_test[index, match(idseries, allinputSeries)]
    
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