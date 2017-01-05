rm(list = ls())
library(dplyr)
source("config_paths.R")


load(file.path(work.folder, "myinfo.Rdata"))
# "myinfoDT", "bottomSeries", "itree", "Sagg", "aggSeries", "n_agg", "n_bottom"

#hierarchy <- "NUTS"

nobs <- 22464

bottom_series <- matrix(NA, nrow = nobs, ncol = n_bottom)
for(j in seq_along(bottomSeries)){
  if(j %% 100 == 0)
    print(j)
  
  idseries <- bottomSeries[j]
  load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))
  bottom_series[, j] <- demand
}
stopifnot(all(!apply(apply(bottom_series, 2, is.na), 2, any)))


agg_series <- tcrossprod(Sagg, bottom_series)

for(iagg in seq(n_agg)){
  demand <- agg_series[iagg, ]
  code <- aggSeries[iagg]
  save(file = file.path(aggseries.folder, paste("series-", code, ".Rdata", sep = "")) , list = c("demand"))
}



# How to compute the child nodes of a node + 
# USEFUL FOR PERMUTATIONS
#subcomponent(itree, 1, "out")[-1]
