rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

library(ff)
load(file.path(work.folder, "myinfo.Rdata"))

ntest <- length(test$id)

agg_MSE_bumcomb <- agg_MSE_bu <- agg_MSE_base <- matrix(NA, nrow = n_agg, ncol = ntest)
bot_MSE_bumcomb <- bot_MSE_bu <- bot_MSE_base <- matrix(NA, nrow = n_bottom, ncol = ntest)

agg_forecasts <- array(NA, c(n_agg, ntest, 5))
bot_forecasts <- array(NA, c(n_bottom, ntest, 5))


for(idtest in seq(ntest)){
  if(idtest%%100 == 0)
  print(idtest)
  
  res_byidtest_file <- file.path(work.folder, "byidtest/byidtest_ICML", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = ""))
  
  #res_byidtest_file <- file.path(work.folder, "byidtest", paste("results_byidtest_", algo.agg, "_", algo.bottom, "_", idtest, ".Rdata", sep = ""))
  load(res_byidtest_file)
  
  
  #  save(file = res_byidtest_file, list = c("QF_agg_idtest", "QF_bottom_idtest", 
  #                                        "obs_agg_idtest", "obs_bottom_idtest",
  #                                        "revisedmean_bottom_idtest", 'revisedmean_agg_idtest',
  #                                        "mean_agg_idtest", "mean_bottom_idtest"))  
  

  
  a_bumcomb_idtest <- Sagg %*% revisedmean_bottom_idtest
  a_bu_idtest <- Sagg %*% mean_bottom_idtest
  agg_MSE_bumcomb[, idtest] <- (obs_agg_idtest - a_bumcomb_idtest)^2
  agg_MSE_base[, idtest] <- (obs_agg_idtest - mean_agg_idtest)^2
  agg_MSE_bu[, idtest] <- (obs_agg_idtest - a_bu_idtest)^2
  
  bot_MSE_bumcomb[, idtest] <- (obs_bottom_idtest - revisedmean_bottom_idtest)^2
  bot_MSE_base[, idtest] <- (obs_bottom_idtest - mean_bottom_idtest)^2
  bot_MSE_bu[, idtest] <- (obs_bottom_idtest - mean_bottom_idtest)^2
  
  agg_forecasts[, idtest, 1] <- obs_agg_idtest
  agg_forecasts[, idtest, 2] <- mean_agg_idtest
  agg_forecasts[, idtest, 3] <- a_bu_idtest
  agg_forecasts[, idtest, 4] <- a_bumcomb_idtest
  agg_forecasts[, idtest, 5] <- revisedmean_agg_idtest
  
  bot_forecasts[, idtest, 1] <- obs_bottom_idtest
  bot_forecasts[, idtest, 2] <- mean_bottom_idtest
  bot_forecasts[, idtest, 3] <- mean_bottom_idtest
  bot_forecasts[, idtest, 4] <- revisedmean_bottom_idtest
  bot_forecasts[, idtest, 5] <- revisedmean_bottom_idtest
  
    
}

iremove <- 86
matplot(t(matrix(agg_MSE_bumcomb[1, ], ncol = 48, byrow = T)[-iremove, ]), type = 'l', lty = 1, main = "BU MCOMB")
matplot(t(matrix(agg_MSE_bu[1, ], ncol = 48, byrow = T)[-iremove, ]), type = 'l', lty = 1, main = "BU")
matplot(t(matrix(agg_MSE_base[1, ], ncol = 48, byrow = T)[-iremove, ]), type = 'l', lty = 1, main = "BASE")

plot.ts(apply(matrix(agg_MSE_base[1, ], ncol = 48, byrow = T)[-iremove, ], 2, mean))
lines(apply(matrix(agg_MSE_bumcomb[1, ], ncol = 48, byrow = T)[-iremove, ], 2, mean), col = "green")
lines(apply(matrix(agg_MSE_bu[1, ], ncol = 48, byrow = T)[-iremove, ], 2, mean), col = "cyan")

#  CHECK THE FORECASTS + check the intercepts

savepdf(file.path(results.folder, paste("ANALYSIS", sep = "") ))
iagg <- 1
for(iday in seq(92)){
  allidtest <- (iday - 1) * 48 + seq(48) 
  v <- agg_forecasts[iagg, allidtest, ]
  matplot(v, type = 'l', lty  = 1, col = c("grey", "black", "cyan", "green", "red"))
}
endpdf()


savepdf(file.path(results.folder, paste("ANALYSIS_BOTTOM", sep = "") ))
ibot <- 1
for(iday in seq(92)){
  allidtest <- (iday - 1) * 48 + seq(48) 
  v <- bot_forecasts[ibot, allidtest, ]
  matplot(v, type = 'l', lty  = 1, col = c("grey", "black", "cyan", "green", "red"))
}
endpdf()

