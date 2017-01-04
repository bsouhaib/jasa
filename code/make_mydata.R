rm(list = ls())
library(lubridate)
library(gdata)
library(dplyr)
source("config_paths.R")
source("config_splitting.R")

makelist <- function(vecintervals){
  sol <- lapply(seq(length(vecintervals)), function(i){list(vecintervals[i])})
  mylist <- lapply(sol, "[[", 1)
}

missingPerDay <- function(meterseries){
  mymat <- matrix(meterseries, ncol = 48, byrow = T)
  ndays <- nrow(mymat)
  vec <- apply(t(apply(mymat, 1, is.na)), 1, sum)
  sort(vec[which(vec != 0)])
}


load(file.path(work.folder, "info.Rdata"))

myregion <- "UKF"
subInfoDT <- infoDT %>%
  filter(NUTS1 %in% myregion)

allintervals <- subInfoDT %>%
  transmute(interval = lubridate::interval(firstAdvance, lastAdvance)) %>%
  .$interval

listintervals <- makelist(allintervals)

myinterval <- interval(startObs, endObs)

matches <- lapply(listintervals, function(oneinterval){ lubridate::intersect(oneinterval, myinterval) == myinterval })
metersInInterval <- subInfoDT[which(unlist(matches)), ] %>% .$IDMETER
print(length(metersInInterval))

pctFound <- numeric(length(metersInInterval)) + NA
listmissing <- vector("list", length(metersInInterval))
for(i in seq_along(metersInInterval)){
  

  
  meter <- metersInInterval[i]
  
  # infoMeter <- infoDT %>% filter(IDMETER == meter) %>% select(firstAdvance, lastAdvance) 
  # firstAdvance <- infoMeter %>% .$firstAdvance
  # lastAdvance  <- infoMeter %>% .$lastAdvance
  # alldates <- seq(firstAdvance, lastAdvance, by = "30 min")
  # ids <- match(myinterval, alldates) 
  # load
  # n_expected <- length(alldates)
  # n_na <- length(which(is.na(dataset[ids, 1])))
  # n_avail <- n_expected - n_na
  # pctFound[i] <- 1 - (n_na/n)
  
  
  load(file.path(initmeters.folder, paste("meter-", meter, ".Rdata", sep = "")))
  
  meterdata <- dataset %>% filter(TIME %within% myinterval) %>% .$"ELECKWH"
  
  n <- length(meterdata)
  n_na <- length(which(is.na(meterdata)))
  pctFound[i] <- 1 - (n_na/n)
  #listmissing[[i]] <- missingPerDay(meterdata)
}