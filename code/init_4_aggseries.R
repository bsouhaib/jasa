rm(list = ls())
library(dplyr)
source("config_paths.R")











# How to compute the child nodes of a node + 
# USEFUL FOR PERMUTATIONS
subcomponent(itree, 1, "out")[-1]