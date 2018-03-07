rm(list = ls())
source("config_paths.R")
source("config_general.R")

library(dplyr)

load(file.path(work.folder, "info.Rdata"))

all_meters <- infoDT %>% .$IDMETER

savepdf(file.path(results.folder, "ISI_meters"))
par(mfrow = c(5, 5), oma = c(.5,.5,0,0) + 0.1,
    mar = c(0,0,.5,.5) + 0.1)
for(i_meter in all_meters[seq(25)]){
  load(file.path(initmeters.folder, paste("meter-", i_meter, ".Rdata", sep = ""))) 

  x <- filter(infoDT, IDMETER == i_meter)
  start_date <- x %>% .$firstAdvance
  last_date <- x %>% .$lastAdvance
  
  seq_complete_interval <- seq(start_date, last_date, by = "30 min")
  
  stopifnot(nrow(dataset) == length(seq_complete_interval))
  
  plot.ts(dataset$ELECKWH[seq(48*2)], xaxt = "n", yaxt = "n")
  abline(v = 48, lty = 2)
  
  #plot.ts(dataset$ELECKWH[seq(48*7*2)], xaxt = "n", yaxt = "n")
  #abline(v = 48*7, lty = 2)
}
dev.off()

savepdf(file.path(results.folder, "ISI_histogram"))
hist(infoDT %>% .$n_expected/48, xlab = expression(T[i]/48), main = "")
dev.off()

#hist(infoDT %>% .$pctFound)
#savepdf(file.path(results.folder, "ISI_histogram"))
#hist(n_all/48, xlab = expression(T[i]/48), main = "")
#dev.off()
