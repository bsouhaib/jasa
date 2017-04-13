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

alldemand <- NULL
res_sort <- sort(apply(Sagg, 1, sum), index = T, decreasing = T)
alliseries <- res_sort$ix
for(iseries in alliseries){
    idseries <- aggSeries[iseries]
    load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))
  alldemand <- cbind(alldemand, demand[seq(48 * 7 + 24)])
}
#id <- seq(1, 55, by = 10)
id <- c(1, 4, 6, 18, 50)
  
alldemand <- alldemand[, id]
load(file.path(mymeters.folder, paste("mymeter-", bottomSeries[20], ".Rdata", sep = "")))
alldemand <- cbind(alldemand, demand[seq(48 * 7 + 24)])
alldemand <- scale(alldemand) 


n <- ncol(alldemand)

offset <- rev(seq(1, n))*5
res <- t(t(alldemand) + offset  )

savepdf(file.path(results.folder, "TSPLOT"))
matplot(res, type = 'n', xlab = "Time", ylab = "", xaxt = "n", yaxt = "n", mgp=c(.5,1,.5))
matlines(res[seq(48 * 7), ], lty = 1)

nb_kids <- c(res_sort$x[id], 1)
for(i in seq_along(nb_kids)){
  text(336 + 23, offset[i], nb_kids[i]) 
}
endpdf()



savepdf(file.path(results.folder, paste("AAAI_", hierarchy, "_aggregates_plot", sep = "")))

alldemand <- NULL
j <- 1

offset <- 30
continue <- TRUE

alloffsets <- offset
allnbnodes <- NULL
while(continue){
  
  cseries <- htscodes[j]
  print(cseries)
  load(file.path(working.folder, paste("/htseries/series-", cseries, ".Rdata", sep = "")))
  
  aggTS <- scale(series[seq(48*8)])
  aggTS <- aggTS + offset
  offset <- offset - 6
  alloffsets <- c(alloffsets, offset)
  
  alldemand <- cbind(alldemand, aggTS)
  
  if(j <=  147){
    children <- childcodes[[j]]
    nbnodes <- length(which(Sagg[j, ] == 1))
    allnbnodes <- c(allnbnodes, nbnodes)
    
    code <- childcodes[[j]][sample(length(children), 1)]
    
    j <- match(code, htscodes)
  }else{
    continue <- FALSE
    allnbnodes <- c(allnbnodes, 1)
  }
}


matplot(head(alldemand, 48*7 + 24), xlab = "Time", ylab = "", type = "n", xaxt = "n", yaxt = "n")
matlines(head(alldemand, 48 * 7), lty = 1, col = "black")
for(i in seq_along(alloffsets)){
  text(336 + 24, alloffsets[i], allnbnodes[i]) 
}

dev.off()