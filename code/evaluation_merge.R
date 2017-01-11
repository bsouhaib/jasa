rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

#stop("PROBLEm:")
load(file.path(work.folder, "myinfo.Rdata"))
#load(file.path(work.folder, "OLDMYINFO.Rdata"))


algo <- "KD-IC-NML"
do.agg <- FALSE
alljobs <- c(1, 2, 3, 4)

set_series <- bottomSeries
if(do.agg)
  set_series <- aggSeries


nseries <- length(set_series)
ntest <- length(test$id)
alliseries <- seq(nseries)
allqcrps <- allmse <- matrix(NA, nrow = nseries, ncol = ntest)

for(ijob in alljobs){
  
  res_crps_file <- file.path(loss.folder, algo, paste("crps_", algo, "_", ijob, ".Rdata", sep = "")) 
  res_mse_file <- file.path(loss.folder, algo, paste("mse_", algo, "_", ijob, ".Rdata", sep = "")) 
  load(res_crps_file) # "crps"
  load(res_mse_file) # "mse"
  
  allqcrps[match(rownames(qcrps), set_series), ] <- qcrps
  allmse[match(rownames(qcrps), set_series), ] <- mse
}

qcrps_series <- apply(allqcrps, 1, mean)

res_crps <- t(sapply(alliseries, function(iseries){
  apply(matrix(allqcrps[iseries, ], ncol = 48, byrow = T), 2, mean)
}))

res_mse <- t(sapply(alliseries, function(iseries){
  apply(matrix(allmse[iseries, ], ncol = 48, byrow = T), 2, mean)
}))

matplot(t(res_crps), lty = 1, type = 'l')
matplot(t(res_mse), lty = 1, type = 'l')

# Bootstrap confidence interval
B <- 100
bres <- matrix(NA, nrow = B, ncol = 48)
for(b in seq(B)){
  bres[b, ] <- apply(res_crps[sample(seq(nrow(res_crps)), replace = T), ], 2, mean)
}
b_mean <- apply(bres, 2, mean)
b_sd <- apply(bres, 2, sd)
matplot(cbind(b_mean - 2 * b_sd, b_mean, b_mean + 2 * b_sd), lty = 1, type = 'l')
lines(apply(res_crps, 2, mean), col = "blue")

#ids <- sort(qcrps_series, decreasing = T, index = T)$ix
#head(ids, 100)
#del <- head(sort(res[, 3], index = T, decreasing = T)$ix, 5)
#matplot(t(res[-del,]), lty = 1, type = "l")
