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

nb_points <- 48 * 7 * 3

alldemand <- NULL
#res_sort <- sort(apply(Sagg, 1, sum), index = T, decreasing = T)
#alliseries <- res_sort$ix

for(iseries in node_order){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  alldemand <- cbind(alldemand, demand[seq(nb_points)])
}
#id <- seq(1, 55, by = 10)
id <- c(1, 4, 7, 39, 52)

  
alldemand <- alldemand[, node_order[id]]
load(file.path(mymeters.folder, paste("mymeter-", bottomSeries[50], ".Rdata", sep = "")))
alldemand <- cbind(alldemand, demand[seq(nb_points)])
#alldemand <- scale(alldemand) 


savepdf(file.path(results.folder, "JASA-METERS"))

colnames(alldemand) <- c(node_nbkids[node_order[id]], 1)
plot.ts(alldemand, nc = 1, main = "", axes = F, ylab = rep("ok", 6), xlab = "Half-hour", xaxt = "n")

#time<-seq(as.Date("2010/1/1"),length.out=1038,by="3 months") 
#axis(1, labels=time, at=seq(from=2010, by=0.25, length.out=length(time)) )

endpdf()


