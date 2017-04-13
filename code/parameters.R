rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

rank <- sort(apply(Sagg, 1, sum), index = T, decreasing = T)$ix

algo <- "DETS"

MAT <- matrix(NA, nrow = n_agg, ncol = 4)
for(i in seq_along(aggSeries)){
  idseries <- aggSeries[i]
  param_file <- file.path(basef.folder, algo, paste("parameters_", idseries, "_", algo, ".Rdata", sep = "")) 
  if(file.exists(param_file)){
    load(param_file)
    MAT[i, ] <- res_optim$par
  }
}
MAT <- MAT[rank, ]

savepdf(file.path(results.folder, paste("DETS", sep = "")))
par(mfrow = c(2, 2))
xlab <- "aggregate series"
plot(MAT[, 1], xlab = xlab, ylab = expression(phi))
plot(MAT[, 2], xlab = xlab, ylab = expression(alpha))
plot(MAT[, 3], xlab = xlab, ylab = expression(delta))
plot(MAT[, 4], xlab = xlab, ylab = expression(omega))
endpdf()

