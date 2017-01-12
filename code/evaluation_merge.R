rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

#stop("PROBLEm:")
load(file.path(work.folder, "myinfo.Rdata"))
#load(file.path(work.folder, "OLDMYINFO.Rdata"))

algorithms <- c("TBATS", "KD-IC-NML")
do.agg <- TRUE
alliseries <- seq(n_agg)

M <- 1128
q_probs <- seq(M)/(M + 1)

ntest <- length(test$id)

if(do.agg){
  set_series <- aggSeries[alliseries]
}else{
  set_series <- bottomSeries[alliseries]
  
  alljobs <- c(1, 2, 3, 4)
}
nseries <- length(set_series)

allqcrps <- allmse <- matrix(NA, nrow = nseries, ncol = ntest)

mat_qs <- array(NA, c(M,  nseries, 48, length(algorithms)))

for(ialgo in seq_along(algorithms)){
  algo <- algorithms[ialgo]
  if(!do.agg){
    for(ijob in alljobs){
      
      res_crps_file <- file.path(loss.folder, algo, paste("crps_", algo, "_", ijob, ".Rdata", sep = "")) 
      res_mse_file <- file.path(loss.folder, algo, paste("mse_", algo, "_", ijob, ".Rdata", sep = "")) 
      load(res_crps_file) # "crps"
      load(res_mse_file) # "mse"
      
      allqcrps[match(rownames(qcrps), set_series), ] <- qcrps
      allmse[match(rownames(qcrps), set_series), ] <- mse
    }
      
  }else{
    
    mat_qs_algo <- array(NA, c(M, ntest, nseries))
    for(i in seq_along(set_series)){
      idseries <- set_series[i]
      qs_file <- file.path(loss.folder, algo, paste("qs_", algo, "_", idseries, ".Rdata", sep = "")) 
      load(qs_file)
      mat_qs_algo[, , i] <- qs_all
    }
    
    v <- sapply(seq(48), function(h){
      apply(mat_qs_algo[, seq(h, ntest, by = 48) , ], c(1, 3), mean)
    }, simplify = "array")
    mat_qs[, , , ialgo] <- v
  }
}

stop("done")

savepdf(file.path(results.folder, paste("JASA_QS", sep = "") ))
par(mfrow = c(3, 3))
# array(NA, c(M,  nseries, 48, length(algorithms)))
for(iagg in seq(n_agg)){
  for(h in c(1, seq(2, 48, by = 7), 48) ){
    matplot(q_probs, mat_qs[, iagg, h, ], lty = 1, type = 'l', col = c("red", "blue"), ylab = "QS", xlab = "prob", main = paste(aggSeries[iagg], " - ", h, sep ="") )
  }
}
dev.off()


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
