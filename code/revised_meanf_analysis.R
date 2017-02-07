rm(list = ls())
source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")
library(igraph)
library(Matrix)
library(glmnet)

load(file.path(work.folder, "myinfo.Rdata"))

algo.agg <- "DYNREG"
algo.bottom <- "KD-IC-NML"

load(file.path(work.folder, "revisedf", paste("revised_meanf_Xmatrix.Rdata", sep = ""))) 

MAT <- matrix(NA, nrow = length(bottomSeries), ncol = length(allinputSeries) + nvar_additional)

for(i in seq_along(bottomSeries)){
  print(i)
  idseries <- bottomSeries[i]
  res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", idseries, ".Rdata", sep = "")) 
  load(res_file)
  
  MAT[i, ] <- as.numeric(model_final$beta)
  #which(as.numeric(model_final$beta) !=0)

}

library(SparseM)
savepdf(file.path(results.folder, paste("Pmatrix-MEANCOMB") ), height = 27 * 0.3)
image(as.matrix.csr(MAT))
dev.off()

MATNOTZERO <- (MAT != 0)
res <- apply(MATNOTZERO, 1, sum)
hist(res)

res_sort <- sort(apply(MATNOTZERO, 2, sum), index = T, decreasing = T)

head(res_sort$x, 20)
allinputSeries[head(res_sort$ix, 20)]


image(as.matrix.csr(MAT[, res_sort$ix]))



