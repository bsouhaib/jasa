rm(list = ls())
set.seed(1987)
args = (commandArgs(TRUE))
if(length(args) == 0){
  do.agg <- F
  alliseries <- c(1, 2, 128) #  c(1406) 
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
print(base::date())

load(file.path(work.folder, "myinfo.Rdata"))

#library(doParallel)
#registerDoParallel(4)

if(do.agg){
  maxit <- 10000
  nlambda <- 100
}else{
  maxit <- 2000
  nlambda <- 50
}
nfolds <- 5
myalpha <- 1

#seth <- vector("list", 2)
#seth[[1]] <- c(1:12, 41:48)
#seth[[1]] <- c(1:10, 43:48)
#seth[[2]] <- setdiff(1:48, seth[[1]])

if(do.agg){
  seth <- vector("list", 3)
  seth[[1]] <- 1:10
  seth[[2]] <- 11: 42
  seth[[3]] <- 43:48
}else{
  seth <- vector("list", 17)
  for(i in seq(10)){seth[[i]] <- i}
  seth[[11]] <- c(11: 42)
  for(i in seq(12, 17)){seth[[i]] <- i + 31}
}

#seth <- vector("list", 1)
#seth[[1]] <- 1:48



do.shrink_towards_base <- TRUE
include.calendar <- TRUE
use.intercept <- TRUE

for(iseries in alliseries){
  print(base::date())
  
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
  #itrain <- seq(round(0.75*nlearn)) 
  #ivalid <- setdiff(seq(nlearn), itrain)
  #Xhat_train <- Xhat_learn[itrain, ]; y_train <- y_learn[itrain]
  #Xhat_valid <- Xhat_learn[ivalid, ]; y_valid <- y_learn[ivalid]
  
  n_test <- nrow(Xhat_test)
  mu_revised_alltest <- numeric(n_test)
  
 
  
  for(i in seq_along(seth)){
    #print(i)
    h_set <- seth[[i]]

    idh_learn <- which(pday_learn %in% h_set)
    Xhat_learn_h <- Xhat_learn[idh_learn, ]
    y_learn_h <- y_learn[idh_learn]
    
    # Reduce size of the dataset by removing far away days
    #nbdays_learn <- nrow(Xhat_learn_h)/length(h_set)
    
    if(FALSE){
      if(i == 1){
        nremove <- 100 * length(h_set)
        Xhat_learn_h <- tail(Xhat_learn_h, -nremove) # 4320 left vs 6320
        y_learn_h    <- tail(y_learn_h,    -nremove)
      }else if(i == 2){
        nremove <- 150 * length(h_set)
        Xhat_learn_h <- tail(Xhat_learn_h, -nremove) # 4648 left vs 8648
        y_learn_h    <- tail(y_learn_h,    -nremove)
      }
    }
    
    
    
    idh_test <- which(pday_test %in% h_set)
    Xhat_test_h <- Xhat_test[idh_test, ]
    #y_test_h <- y_test[idh_test]
    
    # CROSS-VALIDATION
    n <- nrow(Xhat_learn_h)
    foldid <- rep(seq(nfolds), each = ceiling(n/nfolds))
    foldid <- head(foldid, n)
    
    #print("doing glmnet")
    model_seqlambda <- glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, intercept = use.intercept, nlambda = nlambda, maxit = maxit)
    #print("doing cv ...")
    res <- cv.glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, intercept = use.intercept,  foldid = foldid, lambda = model_seqlambda$lambda, maxit = maxit)
    best_lambda <-  res$lambda.1se
    
    ids_nonzero <- which(as.numeric(coef(model_seqlambda, s = best_lambda)) != 0)
    if( (!do.agg && do.shrink_towards_base) && length(ids_nonzero) == 1 && ids_nonzero == 1 ){
      mu_revised_alltest[idh_test] <- 0
    }else{
      mu_revised_alltest[idh_test] <- as.numeric(predict(model_seqlambda, newx = Xhat_test_h, s = best_lambda))
    }
    
    if(FALSE){
      #model_seqlambda <- glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, intercept = use.intercept, nlambda = nlambda)
      #res <- cv.glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, 
      #                 intercept = use.intercept,  foldid = foldid, lambda = model_seqlambda$lambda)
      res <- cv.glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, intercept = use.intercept,  foldid = foldid, nlambda = nlambda)
      best_lambda <-  res$lambda.1se
      
      model_final <- glmnet(y = y_learn_h, x = Xhat_learn_h, alpha = myalpha, 
                            lambda = best_lambda, intercept = use.intercept)
      mu_revised_alltest[idh_test] <- as.numeric(predict(model_final, Xhat_test_h))
    }
    #beta <- as.numeric(model_final$beta)
    #id <- which(beta != 0)
    #print("---")
    #print(id)
    #print("---")
    
    if(!do.agg && do.shrink_towards_base){
      mu_revised_alltest[idh_test] <- mu_revised_alltest[idh_test] + Xhat_test_h[, col_iseries]
    }
  }
  
  #print(warnings())
  #browser()
  #stop("done")
  
  res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", algo.agg, "_", algo.bottom, "_", idseries, ".Rdata", sep = "")) 
  save(file = res_file, list = c("mu_revised_alltest"))
}
