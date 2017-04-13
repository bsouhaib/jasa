rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")

load(file.path(work.folder, "myinfo.Rdata"))

node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

day_dweek <- matrix(calendar$dweek, ncol = 48, byrow = T)[, 1]
dweeks <- unique(day_dweek)

# all meters
savepdf(file.path(results.folder, paste("cycle_all_meters", sep = "")))
par(mfrow = c(2, 2))
set_ids <- seq(n_bottom)
for(i in set_ids){
  idseries <- bottomSeries[i]
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  demand <- demand/max(demand)
  demand_day <- matrix(demand, ncol = 48, byrow = T)
  res <- sapply(dweeks, function(dweek){
    apply(demand_day[which(day_dweek == dweek), ], 2, mean)
  })
  matplot(res, type = 'l', lty = 1)
}
endpdf()


# average over all meters
MAT <- array(NA, c(n_bottom, 48, 7)) 
#matrix(NA, nrow = n_bottom, ncol = 48)

for(iseries in seq(n_bottom)){
  idseries <- bottomSeries[iseries]
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  demand <- demand/max(demand)
  demand_day <- matrix(demand, ncol = 48, byrow = T)
  
  res <- sapply(dweeks, function(dweek){
    apply(demand_day[which(day_dweek == dweek), ], 2, mean)
  })
  MAT[iseries, , ] <- res
}

savepdf(file.path(results.folder, paste("cycle_meters", sep = "")), height = 27 * 0.25, width = 21)
par(mfrow = c(1, 3))

# average over meters
v <- apply(MAT, c(2, 3), mean)
matplot(v, type = 'l', lty = 1, xlab = "", ylab = "Electricity demand (scaled)")

set_ids <- c(1223, 1233)
for(i in set_ids){
  idseries <- bottomSeries[i]
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  demand <- demand/max(demand)
  demand_day <- matrix(demand, ncol = 48, byrow = T)
  res <- sapply(dweeks, function(dweek){
    apply(demand_day[which(day_dweek == dweek), ], 2, mean)
  })
  matplot(res, type = 'l', lty = 1, ylab = "Electricity demand (scaled)")
}
endpdf()

savepdf(file.path(results.folder, paste("cycle_agg", sep = "")), height = 27 * 0.25, width = 21)

for(iagg in node_order){
  x <- MAT[which(Sagg[iagg, ] == 1), , ]
  v <- apply(x, c(2, 3), mean)
  matplot(v, type = 'l', lty = 1, xlab = "", ylab = "Electricity demand (scaled)")
}
endpdf()


