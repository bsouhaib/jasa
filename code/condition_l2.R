rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
#library(parallel)
library(igraph)

load(file.path(work.folder, "myinfo.Rdata"))

idseries <- bottomSeries[1020]
resid_file <- file.path(insample.folder, algo.bottom, 
                        paste("residuals_", idseries, "_", algo.bottom, ".Rdata", sep = ""))
load(resid_file)

v <- matrix(e_residuals, nrow = 336 * 4)
boxplot(v, outline = F)

plot.ts(e_residuals)



#e_residuals <- c(rep(NA, n_past_obs_kd), e_residuals)

idseries <- aggSeries[15]
resid_file <- file.path(insample.folder, algo.agg, 
                        paste("residuals_", idseries, "_", algo.agg, "_", 1, ".Rdata", sep = "")) 
load(resid_file)

v <- matrix(e_residuals, nrow = 336 * 4)
boxplot(v, outline = F)

plot.ts(e_residuals)



stop("done")
if(isbottom){
  resid_file <- file.path(insample.folder, algo.bottom, paste("residuals_", idseries, "_", algo.bottom, ".Rdata", sep = "")) 
  load(resid_file)
  e_residuals <- c(rep(NA, n_past_obs_kd), e_residuals)
}else{
  resid_file <- file.path(insample.folder, algo.agg, paste("residuals_", idseries, "_", algo.agg, "_", 1, ".Rdata", sep = "")) 
  load(resid_file)
  #e_residuals
}
e_residuals

