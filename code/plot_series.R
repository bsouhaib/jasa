rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")

library(parallel)
library(fBasics)
library(msm)
library(gtools)
library(forecast)
library(abind)
library(glmnet)

load(file.path(work.folder, "myinfo.Rdata"))

node_nbkids <- apply(Sagg, 1, sum)
node_order <- sort(node_nbkids, index = T, decreasing = T)$ix

offset <- 48 * 7 * 4

#nb_points <- 48 * 7 * 3
nb_points <- 48 * 7 * 2
nb_points <- 48 * 7 

alldemand <- NULL
#res_sort <- sort(apply(Sagg, 1, sum), index = T, decreasing = T)
#alliseries <- res_sort$ix

for(iseries in node_order){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  alldemand <- cbind(alldemand, demand[offset + seq(nb_points)])
}
#id <- seq(1, 55, by = 10)
#id <- c(1, 4, 7, 39, 52)

id <- c(1, 7, 39, 50)

  
alldemand <- alldemand[, node_order[id]]
load(file.path(mymeters.folder, paste("mymeter-", bottomSeries[511], ".Rdata", sep = "")))
alldemand <- cbind(alldemand, demand[seq(nb_points)])
#alldemand <- scale(alldemand) 
colnames(alldemand) <- c(node_nbkids[node_order[id]], 1)

#savepdf(file.path(results.folder, "JASA-METERS"))
#plot.ts(alldemand, nc = 1, main = "", axes = F, xlab = "Half-hour", xaxt = "n")
#abline(v = 48 * 7, lwd = .5)
#endpdf()

#time<-seq(as.Date("2010/1/1"),length.out=1038,by="3 months") 
#axis(1, labels=time, at=seq(from=2010, by=0.25, length.out=length(time)) )


savepdf(file.path(results.folder, "demand-bylevel"))
my.ts.panel <- function(x, col = col, bg = bg, pch = pch, type = type,  vpos=48*7, ...){
  lines(x, col = col, bg = bg, pch = pch, type = type, ...)
  #abline(v=vpos)
  }
plot.ts(alldemand, panel=my.ts.panel,  nc = 1, axes = F, xlab = "Half-hour", xaxt = "n", main = "")
endpdf()
# main = paste(node_nbkids[node_order[id]], " aggregated meters", sep = ""), 