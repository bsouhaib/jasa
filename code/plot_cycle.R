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

if(FALSE){
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
}


####
MAT_bottom <- array(NA, c(n_bottom, 48, 7)) 
MAT_agg <- array(NA, c(n_agg, 48, 7)) 
#matrix(NA, nrow = n_bottom, ncol = 48)

for(iseries in seq(n_bottom)){
  idseries <- bottomSeries[iseries]
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  demand_day <- matrix(demand, ncol = 48, byrow = T)
  
  res <- sapply(dweeks, function(dweek){
    apply(demand_day[which(day_dweek == dweek), ], 2, mean)
  })
  MAT_bottom[iseries, , ] <- res
}
for(iseries in seq(n_agg)){
  idseries <- aggSeries[iseries]
  load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  demand_day <- matrix(demand, ncol = 48, byrow = T)
  
  res <- sapply(dweeks, function(dweek){
    apply(demand_day[which(day_dweek == dweek), ], 2, mean)
  })
  MAT_agg[iseries, , ] <- res
}

#pdf(file.path(results.folder, paste("IC.pdf", sep = "")), width = 21 * 0.9, height = 27 * 0.5)
#par(mfrow = c(2, 3))

do.slides <- TRUE
if(do.slides){
  savepdf(file.path(results.folder, paste("ICslides", sep = "")))
  par(mfrow = c(2, 3))
}else{
  savepdf(file.path(results.folder, paste("IC", sep = "")), height = 27 * 0.5)
  par(mfrow = c(3, 2))
}


itday <- c(1, seq(8, 48, by = 8))

# id <- node_order
#id <- c(1, 4, 7, 39, 52)
id <- c(1, 7, 39, 50)

my_cex <- .7
my_pch <- c(3, 4, 5, 6, 7, 1, 2)
for(iagg in node_order[id]){
  ind <- which(Sagg[iagg, ] == 1)
  v <- MAT_agg[iagg, , ]
  #v <- apply(x, c(2, 3), mean)
  # matplot(v, type = 'l', lty = my_lty, ylab = "Electricity demand", main = length(ind), xaxt = "n", xlab = "Time of Day", col = "black")
  matplot(v, ylab = "Consumption (kWh)", main = length(ind), xaxt = "n", xlab = "Time of Day", col = "black", type = 'l')
  matpoints(v, pch = my_pch, col = "black", cex = my_cex)
  #axis(1, labels = tday[seq(1, 48, by = 8)], at = seq(1, 48, by = 8))
  axis(1, labels = tday[itday], at = itday)
  if(iagg == 1){
    legend("topleft", abbr.dweek[c(6, 7, 1, 2, 3, 4, 5)], pch = my_pch[c(6, 7, 1, 2, 3, 4, 5)], lty = 1, cex = my_cex, bty = "n")
  }
}

#matplot(MAT[50, , ], type = 'l', lty = my_lty, ylab = "Electricity demand (scaled)", main = 1, xaxt = "n", xlab = "Time of Day", col = "black")
matplot(MAT_bottom[50, , ], ylab = "Electricity demand", main = 1, xaxt = "n", xlab = "Time of Day", col = "black", type = 'l')
matpoints(MAT_bottom[50, , ], pch = my_pch, col = "black", cex = my_cex)
axis(1, labels = tday[itday], at = itday)
#axis(1, labels = tday[seq(1, 48, by = 8)], at = seq(1, 48, by = 8))

dev.off()


