rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)
library(Matrix)

library(quantreg)
library(rqPen)

load(file.path(work.folder, "myinfo.Rdata"))



algo.agg <- "DYNREG"
algo.bottom <- "KD-IC-NML"

idseries <- bottomSeries[1300]
idseries <- aggSeries[1]

if(idseries %in% aggSeries){
  load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
}else{
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
}
demand_idseries <- demand

allSeries <- c(bottomSeries, aggSeries)
#allSeries <- c(aggSeries)
#allSeries <- c(aggSeries, bottomSeries[seq(500)])

#nbpast <- 48 * 7 * 4 * 3 + length(validation$id)
#idt <- tail(learn$id, nbpast)
ids_past <- learn$id
nbpast <- length(ids_past)
Xhat_learn <- matrix(NA, nrow = nbpast, ncol = length(allSeries) + 1)
y_learn <- demand_idseries[ids_past]

X_test <- matrix(NA, nrow = length(test$id), ncol = length(allSeries) + 1)
y_test <- demand[test$id]

  
for(i_xvar in seq_along(allSeries)){
  
  xvar_series <- allSeries[i_xvar]
  print(xvar_series)
  
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
  res_file <- file.path(basef.folder, algo, paste("results_", xvar_series, "_", algo, ".Rdata", sep = "")) 

  load(insample_condmean_file)
  load(res_file) # all_mf
  
  if(algo == "KD-IC-NML"){
    all_mu <- unlist(all_mu)  
    all_mu <- c(rep(NA, n_past_obs_kd), all_mu)
  }
  all_mu 
  
  Xhat_learn[, i_xvar] <-  all_mu
  
  X_test[, i_xvar] <- unlist(all_mf)
}

Xhat_learn[, ncol(Xhat_learn)] <- as.factor(calendar$periodOfDay[learn$id])
X_test[, ncol(X_test)] <- as.factor(calendar$periodOfDay[test$id])

# remove na
ikeep <- which(complete.cases(Xhat_learn))
Xhat_learn <- Xhat_learn[ikeep, ]
y_learn <- y_learn[ikeep]

n <- nrow(Xhat_learn)
itrain <- seq(round(0.75*n)) 
ivalid <- setdiff(seq(n), itrain)

Xhat_train <- Xhat_learn[itrain, ]; y_train <- y_learn[itrain]
Xhat_valid <- Xhat_learn[ivalid, ]; y_valid <- y_learn[ivalid]

myalpha <- 1
model <- glmnet(y = y_train, x = Xhat_train, alpha = myalpha)

pred <- predict(model, Xhat_valid)
val.err <- apply((pred - y_valid)^2, 2, mean)
plot.ts(val.err)

best_lambda <- model$lambda[which.min(val.err)]

model_final <- glmnet(y = y_learn, x = Xhat_learn, alpha = myalpha, lambda = best_lambda)

# testing

pred_test <- as.numeric(predict(model_final, X_test))

err_new <- err_old <- NULL
savepdf(file.path(results.folder, paste("PREDTEST", sep = "") ))
for(ijob in seq(92)){
  index <- (ijob - 1) * 48 +  seq(48)
  new_pred <- pred_test[index]
  actual <- demand[test$id[index]]
  old_pred <- X_test[index, match(idseries, allSeries)]

  err_new <- c(err_new, (new_pred - actual)^2)
  err_old <- c(err_old, (old_pred - actual)^2)
  
  matplot(cbind(new_pred, actual, old_pred), type = 'l', col = c("blue", "black", "red"))
}
dev.off()


X <- NULL
X <- cbind(X, apply(matrix(err_new, ncol = 48, byrow = T), 2, mean))
X <- cbind(X, apply(matrix(err_old, ncol = 48, byrow = T), 2, mean))
matplot(X, type = 'l', lty = 1, col = c("blue", "red"))

  
#plot.ts(head(y_train, 48 * 7 *2))
#lines(head(predict(model, X_hat_train)[, 100],  48 * 7 *2), col = "red")


