rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

algo.bottom  <- "KD-IC-NML"
algo.agg <- "TBATS"

allmethods_samples_agg <- array(NA, c(n_agg, ntest, length(methods)))
for(idjob in 1:4){
  res_job <- file.path(loss.folder, "HTS", paste("results_HTS_", algo.agg, "_", algo.bottom, "_", idjob, ".Rdata", sep = "")) 
  load(res_job)
  
  list_crps <- list_crps[-which(sapply(list_crps, is.null))]
  mat_res <- sapply(seq_along(list_crps),  function(i){list_crps[[i]]}, simplify = 'array')
  
  allidtest <- (idjob - 1) * 1104 + seq(1104) 
  allmethods_samples_agg[, allidtest, ] <- mat_res
}

res2 <- sapply(seq(n_agg), function(iagg){
  sapply(seq_along(methods), function(imethod){
    apply(matrix(res[iagg, imethod, ], ncol = 48, byrow = T), 2, mean)
  })
}, simplify = 'array')

