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

nb_points <- 48 * 7 * 4
nb_end <- 30

alldemand <- NULL
res_sort <- sort(apply(Sagg, 1, sum), index = T, decreasing = T)
alliseries <- res_sort$ix
for(iseries in alliseries){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  alldemand <- cbind(alldemand, demand[seq(nb_points + nb_end)])
}
#id <- seq(1, 55, by = 10)
id <- c(1, 4, 7, 39, 52)
  
alldemand <- alldemand[, id]
load(file.path(mymeters.folder, paste("mymeter-", bottomSeries[50], ".Rdata", sep = "")))
alldemand <- cbind(alldemand, demand[seq(nb_points + nb_end)])
alldemand <- scale(alldemand) 


n <- ncol(alldemand)

#offset <- rev(seq(1, n))*5
#offset[length(offset)] <- 10

offset <- c(47, 42, 35, 29, 20, 10)

res <- t(t(alldemand) + offset  )

#savepdf(file.path(results.folder, "TSPLOT"))
savepdf(file.path(results.folder, "JASA-METERS"))

matplot(res, type = 'n', xlab = "Time", ylab = "", xaxt = "n", yaxt = "n", mgp=c(.5,1,.5))
matlines(res[seq(nb_points), ], lty = 1)

nb_kids <- c(res_sort$x[id], 1)
for(i in seq_along(nb_kids)){
  text(nb_points + 87, offset[i], nb_kids[i], cex = 0.7) 
}
endpdf()


