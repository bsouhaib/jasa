rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

algo <- "KD-IC-NML"

ids <- tail(learn$id, -48 * 90)
nresid <- length(ids)

do.agg <- TRUE
alliseries <- c(6, 7)

mat_residhat <- mat_muhat <- mat_varhat  <- mat_demand <- matrix(NA, nrow = nresid, ncol = length(alliseries))

#mat_demand <- matrix(NA, nrow = 22464, ncol = length(alliseries))

for(i in seq_along(alliseries)){
  iseries <- alliseries[i]

  if(do.agg){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  }else{
    idseries <- bottomSeries[iseries]
    load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  }
  mat_demand[, i] <- demand[tail(learn$id, -48 * 90)]
  
  res_file <- file.path(basef.folder, algo, paste("results_", idseries, "_", algo, ".Rdata", sep = "")) 
  load(res_file)
  
  for(idresid in seq(nresid)){
    print(idresid)
    iday <- getInfo(idresid)$iday
    hour <- getInfo(idresid)$hour
    
    if(hour %in% hours_night){
      index <- match(hour, hours_night)
      residhat <- res_residuals$res_nighthours[[iday]][[index]]$residuals
      muhat <- res_residuals$res_nighthours[[iday]][[index]]$mu_hat
      varhat <- res_residuals$res_nighthours[[iday]][[index]]$var_hat
    }else{
      index <- match(hour, hours_day)
      residhat <- res_residuals$res_dayhours[[iday]][[index]]$residuals
      muhat <- res_residuals$res_dayhours[[iday]][[index]]$mu_hat
      varhat <- res_residuals$res_dayhours[[iday]][[index]]$var_hat
    }
    mat_residhat[idresid, i] <- residhat
    mat_muhat[idresid, i] <- muhat
    mat_varhat[idresid, i] <- varhat
  }
  
  #plot.ts(demand[head(ids, 48 * 30)], main = idseries)
  #lines(mat_muhat[seq(48*30), i], col = "red")

}
#ret <- list(residuals = residuals, squared_error = squared_error, mu_hat = mu_hat, var_hat = var_hat)

savepdf(file.path(results.folder, paste("COPULA", sep = "") ))
U <- sapply(seq(ncol(mat_residhat)), function(j){ecdf(mat_residhat[, j])(mat_residhat[, j])})
for(h in seq(48)){
  print(h)
  plot(U[seq(h, nrow(U), by = 48), ])
}
dev.off()

U2 <- sapply(seq(ncol(mat_demand)), function(j){ecdf(mat_demand[, j])(mat_demand[, j])})

savepdf(file.path(results.folder, paste("COPULA-data", sep = "") ))
for(h in seq(48)){
  print(h)
  plot(U2[seq(h, nrow(U), by = 48), ])
}
dev.off()



X <- c(3.1, 6.3, 1.4, 5.9)
Y <- c(67.9, 12.2, 22.8, 43.7)

U1 <- c(0.4, 0.5, 0.1, 0.7)
U2 <- c(0.7, 0.9, 0.3, 0.4)
U <- cbind(U1, U2)

Xnew <- sort(X)[rank(U1)]
Ynew <- sort(Y)[rank(U2)]

Xnew <- X
Ynew <- Y
Xnew[order(U1)] = sort(X)
Ynew[order(U2)] = sort(Y)


ii <- seq(48*30)
ii <- seq(nrow(mat_muhat) - 48 * 30, nrow(mat_muhat))

ii <- 48*30 * 1 + seq(48 * 30)
plot.ts(mat_demand[ii, 2], main = idseries)
lines(mat_muhat[ii, 2], col = "red")

