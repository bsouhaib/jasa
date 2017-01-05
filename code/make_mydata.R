rm(list = ls())
library(lubridate)
library(gdata)
library(dplyr)
library(igraph)
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

# "UKG" "UKL" "UKJ" "UKI" "UKM" "UKF" "---" "UKK" "UKD"
myregion <- "UKF"
#myregion <- "UKG"
#myregion <- "UKJ"

subInfoDT <- infoDT %>%
  filter(NUTS1 %in% myregion)

allintervals <- subInfoDT %>%
  transmute(interval = lubridate::interval(firstAdvance, lastAdvance)) %>%
  .$interval

listintervals <- makelist(allintervals)

myinterval <- interval(startObs, endObs)
seq_myinterval <- seq(startObs, endObs, by = "30 min")

matches <- lapply(listintervals, function(oneinterval){ lubridate::intersect(oneinterval, myinterval) == myinterval })
metersInInterval <- subInfoDT[which(unlist(matches)), ] %>% .$IDMETER
print(length(metersInInterval))


pctFound <- n <- n_na <- n_expected <- n_avail <- numeric(length(metersInInterval)) + NA
listmissing <- vector("list", length(metersInInterval))
for(i in seq_along(metersInInterval)){
  print(i)
  meter <- metersInInterval[i]
  
  infoMeter <- subInfoDT %>% filter(IDMETER == meter) %>% select(firstAdvance, lastAdvance) 
  firstAdvance <- infoMeter %>% .$firstAdvance
  lastAdvance  <- infoMeter %>% .$lastAdvance
  alldates <- seq(firstAdvance, lastAdvance, by = "30 min")
  ids <- match(seq_myinterval, alldates) 
  stopifnot(all(!is.na(ids)))

  load(file.path(initmeters.folder, paste("meter-", meter, ".Rdata", sep = "")))
  n[i] <- nrow(dataset)
  n_expected[i] <- length(alldates)
  n_na[i] <- length(which(is.na(dataset[ids, 1])))
  n_avail[i] <- n_expected[i] - n_na[i]
  pctFound[i] <- 1 - (n_na[i]/n[i])
}

id <- which(pctFound > 0.99)
finalmeters <- metersInInterval[id]



# Keep meters with few missing values
x <- subInfoDT %>% filter(IDMETER %in% finalmeters)

# Keep meters with complete NUTS information
x <- x %>% filter(!grepl("-", NUTS4))

# Remove few weird meters
x <- x %>% filter(!IDMETER %in% c(6228, 13154, 9503))

# Each node must have at least two children nodes (NUTS HIERARCHY)
idset <- which(x[, "NUTS4"] == "UKF2100")
res <- split(idset, c(1,2))
x[res[[1]], "NUTS4"] <- "UKF2100"
x[res[[2]], "NUTS4"] <- "UKF2101"

idset <- which(x[, "NUTS4"] == "UKF2202")
res <- split(idset, c(1,2))
x[res[[1]], "NUTS4"] <- "UKF2202"
x[res[[2]], "NUTS4"] <- "UKF2209"

idset <- which(x[, "NUTS4"] == "UKF1100")
res <- split(idset, c(1,2))
x[res[[1]], "NUTS4"] <- "UKF1100"
x[res[[2]], "NUTS4"] <- "UKF1101"

idset <- which(x[, "NUTS4"] == "UKF1400")
res <- split(idset, c(1,2))
x[res[[1]], "NUTS4"] <- "UKF1400"
x[res[[2]], "NUTS4"] <- "UKF1401"

for(mynuts4 in c("UKF3004", "UKF3006")){
  idset <- which(x[, "NUTS4"] == mynuts4)
  x[idset, "NUTS4"] <- paste("UKF31", substr(mynuts4, 6,7), sep = "")
  x[idset, "NUTS3"] <- "UKF31"
}

# Remove some branches in DEMO HIERARCHY
x <- x %>% filter(DEMO2 != "D517", DEMO1 != "D2")

# Save myinfo.Rdata
myinfoDT <- x
mymeters <- x %>% .$IDMETER


# save(file = file.path(working.folder, "myinfo.Rdata") , list = c("myinfoDT", "mymeters"))

stop("done")
############################
pdf(paste("NUTS-", myregion, ".pdf", sep= ""))
#myDT <- dplyr::filter(subInfoDT, IDMETER %in% finalmeters)
myDT <- x
c <- data.frame(rbind(cbind(myregion,myDT$NUTS2), cbind(myDT$NUTS2, myDT$NUTS3), cbind(myDT$NUTS3, myDT$NUTS4) ))
g <- graph.data.frame(c)
g <- simplify(g, remove.loops = F)
plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.label.cex = 0.4, vertex.size = 1, vertex.label.dist = .2)
dev.off()

#print(dim(subInfoDT %>% filter(IDMETER %in% finalmeters)))
#print(as.data.frame(subInfoDT %>% filter(IDMETER %in% finalmeters) %>% count(NUTS1, NUTS4) %>% arrange(n)))

print(dim(x))
print(as.data.frame(x %>% count(NUTS1, NUTS4) %>% arrange(n)))


pdf(paste("DEMO-", myregion, ".pdf", sep= ""))
c <- data.frame(rbind(cbind("DEMO",myDT$DEMO1), cbind(myDT$DEMO1, myDT$DEMO2), cbind(myDT$DEMO2, myDT$DEMO3) ))
g <- graph.data.frame(c)
g <- simplify(g, remove.loops = F)
plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.label.cex = 0.4, vertex.size = 1, vertex.label.dist = .2)
dev.off()
#print(as.data.frame(subInfoDT %>% filter(IDMETER %in% finalmeters) %>% count(DEMO1, DEMO3) %>% arrange(n)))

#print(as.data.frame(x %>% count(DEMO1, DEMO3) %>% arrange(n)))


# scp -i "bsouhaib-key-pair-sydney.pem" ubuntu@ec2-13-54-209-237.ap-southeast-2.compute.amazonaws.com:/home/rstudio/codemeters/code/Rplots.pdf .


