rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

algo.agg <- "TBATS"
algo.bottom  <- "KD-IC-NML"

ntest <- length(test$id)
n_bottom <- length(bottomSeries)

nbperjob <- 276
njobs <- ntest/nbperjob

crps_agg    <- array(NA, c(n_agg, ntest, 5))
crps_bottom <- array(NA, c(n_bottom, ntest, 2))

total_qscores_agg <- total_qscores_bot <- 0

for(idjob in seq(njobs)){
  allidtest <- (idjob - 1) * nbperjob + seq(nbperjob) 
  
  res_job <- file.path(loss.folder, "HTS", paste("results_HTS_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
  load(res_job)
  
  list_crps_agg_nonull <- list_crps_agg[-which(sapply(list_crps_agg, is.null))]
  mat_crps_agg <- sapply(seq_along(list_crps_agg_nonull),  function(i){list_crps_agg_nonull[[i]]}, simplify = 'array')
  
  list_crps_bot_nonull <- list_crps_bot[-which(sapply(list_crps_bot, is.null))]
  mat_crps_bot <- sapply(seq_along(list_crps_bot_nonull),  function(i){list_crps_bot_nonull[[i]]}, simplify = 'array')
  
  #allmethods_samples_agg[, allidtest, ] <-  aperm(mat_res, c(1, 3, 2))
  
  crps_agg[, allidtest,]     <- aperm(mat_crps_agg, c(1, 3, 2))
  crps_bottom[, allidtest, ] <- aperm(mat_crps_bot, c(1, 3, 2))
  total_qscores_agg <- total_qscores_agg + avg_qscores_agg
  total_qscores_bot <- total_qscores_bot + avg_qscores_bot

}

total_qscores_agg <- total_qscores_agg / njobs
total_qscores_bot <- total_qscores_bot / njobs

# crps_agg   total_qscores_agg
# crps_bottom total_qscores_bot

stop("done")

# COMPUTE BOOTSTRAPPED STANDARD ERROR

res2 <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(methods), function(imethod){
    apply(matrix(allmethods_samples_agg[iagg, , imethod], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

savepdf(file.path(results.folder, paste("RESPLOT", sep = "") ))
for(iagg in seq(n_agg)){
  matplot(res2[, , iagg], type = 'l', col = c("black", "red", "blue"), lty = 1)
}
dev.off()
