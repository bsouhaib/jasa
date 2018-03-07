rm(list = ls())
source("config_paths.R")
source("config_general.R")
library(dplyr)

# compute features for each meter
load(file.path(work.folder, "info.Rdata"))
all_meters <- infoDT %>% .$IDMETER

#n <- 100
n <- length(all_meters)

MAT <- matrix(NA, nrow = n, ncol = 24)

my_meters <- all_meters[seq(n)]
#my_meters <- setdiff(my_meters, c(2602, 2945) )

for(i_meter in seq_along(my_meters)){
  if(i_meter %% 100 == 0){
    print(i_meter)
  }
  
  
  idmeter <- my_meters[i_meter]
  load(file.path(initmeters.folder, paste("meter-", idmeter, ".Rdata", sep = "")))
  
  
  x <- filter(infoDT, IDMETER == idmeter)
  start_date <- x %>% .$firstAdvance
  last_date <- x %>% .$lastAdvance
  
  if(int_length(lubridate::interval(start_date, last_date))/3600 > 24*7){
    
    seq_complete_interval <- seq(start_date, last_date, by = "30 min")
    calendar <- NULL
    calendar$dweek <- lubridate::wday(seq_complete_interval)
    calendar$dweek <- (calendar$dweek - 1) + ((calendar$dweek - 1) == 0)*7
    calendar$periodOfDay <- 2*(lubridate::hour(seq_complete_interval) + 1) - (lubridate::minute(seq_complete_interval) == 0)
    calendar$tyear <- lubridate::yday(seq_complete_interval)
    calendar$periodOfWeek <- (calendar$dweek - 1)*48 + calendar$periodOfDay
    calendar$year <- lubridate::year(seq_complete_interval)
    
    idfirst <- which(calendar$periodOfDay[seq(48*2)] == 1)[1]
    
    
    demand <- dataset$ELECKWH[seq(idfirst, length(seq_complete_interval))]
    nb_last_toremove <- length(demand)%%48
    if(nb_last_toremove > 0){
      demand <- head(demand, -nb_last_toremove)
    }
    
    shapes <- matrix(demand, ncol = 48, byrow = TRUE)		
    shapes <- sapply(seq(1, ncol(shapes)-1, by = 2), function(j){apply(shapes[, c(j, j+1)], 1, sum) })
    
    dtotal <- apply(shapes, 1, sum)
    vec <- apply(t(t(shapes) / dtotal), 2, median, na.rm = T)
    
    MAT[i_meter, ] <- vec
    
  }
  #if(length(demand)%%48 != 0){
  #  browser()
  #}
  
  #if(calendar$periodOfDay[1] != 1){
  #  browser()
  #}
  
  #stop("done")
  
  #dataset$ELECKWH

}

pctFound <- infoDT %>% .$pctFound
n_expected <- infoDT %>% .$n_expected
  
MAT2 <- cbind(MAT, pctFound[seq(n)], n_expected[seq(n)])
row.names(MAT2) <- all_meters
#MAT2 <- cbind(MAT, n_expected[seq(n)])

#colnames("MAT") <- c(seq(24), "pct", "n")

MAT_FINAL <- MAT2[-unique(which(is.na(MAT2), arr.ind = T)[, 1]), ]
MAT_FINAL <- MAT_FINAL[-13578, ]

library(tsexplore)
library(ggplot2)

savepdf(file.path(results.folder, paste("ISI_METERS_PCA") ))
obj <- reducedim(MAT_FINAL, method = "PCA", retpca = TRUE)
p1   <- getplot(obj, colouring = "custom", colours = 1, pca.plotarrow = TRUE) + 
  ggtitle("") + 
  theme(legend.position="none")
print(p1)
dev.off()

set.seed(1986)

for(i in seq(4)){
savepdf(file.path(results.folder, paste("ISI_METERS_PCA_", i, sep = "") ))

scores <- obj$pca$scores
#ids <- which(scores[, 1] < -5) 
#ids <- sample(sort(scores[, 1], index = T)$ix, 6)

if(i == 1){
  ids <- sample(which(scores[, 1] < -10), 3)
}else if(i == 2){
  ids <- sample(which(scores[, 2] > 5), 3)
}else if(i == 3){
  ids <- sample(which(scores[, 2] < -7), 3)
}else if(i == 4){
  ids <- sample(which(abs(scores[, 2]) < 2 & abs(scores[, 1]) < 2), 6)
}

SCORES <- data.frame(obj$pca$scores)
colnames(SCORES) <- c("PC1", "PC2")
print(p1 + 
        geom_point(data=SCORES[ids, ], aes(x=PC1, y=PC2), colour="orange", size = 3)
)
dev.off()

savepdf(file.path(results.folder, paste("ISI_METERS_shapes_", i, sep = "") ), height = 5)
par(mfrow = c(1, 3))
for(i in ids){
  print(i)
  plot.ts(MAT_FINAL[i, -c(25, 26)], ylab = "", xlab = "Time of day")
  
  # row.names(MAT_FINAL)[i]
}
dev.off()
}

