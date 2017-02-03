rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)
library(Matrix)

load(file.path(work.folder, "myinfo.Rdata"))

savepdf(file.path(results.folder, paste("hierarchy", sep = "") ))

plot(itree, layout = 
       layout.reingold.tilford(itree, root=1, circular=T), vertex.size=0, vertex.label=NA, edge.arrow.size=0)
dev.off()