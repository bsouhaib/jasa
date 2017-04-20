rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")

load(file.path(work.folder, "myinfo.Rdata"))


node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

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
MAT <- MAT[node_order, ]

savepdf(file.path(results.folder, paste("DETS", sep = "")))
par(mfrow = c(2, 2))
xlab <- "aggregate series"
plot(MAT[, 1], xlab = xlab, ylab = expression(phi))
plot(MAT[, 2], xlab = xlab, ylab = expression(alpha))
plot(MAT[, 3], xlab = xlab, ylab = expression(delta))
plot(MAT[, 4], xlab = xlab, ylab = expression(omega))
endpdf()


savepdf(file.path(results.folder, paste("DETS", sep = "")))
par(mfrow = c(2, 2))
xlab <- "number of aggregated meters (log scale) "
for(j in seq(4)){
  if(j == 1){
    my_ylab <- expression(phi)
  }else if(j == 2){
    my_ylab <- expression(alpha)
  }else if(j == 3){
    my_ylab <- expression(delta)
  }else if(j == 4){
    my_ylab <- expression(omega)
  }
  plot(log(node_nbkids), MAT[, j], xlab = xlab, ylab = my_ylab)
}
endpdf()

