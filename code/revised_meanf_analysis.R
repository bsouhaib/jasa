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

allSeries <- c(aggSeries, bottomSeries)
MAT <- matrix(NA, nrow = length(allSeries), ncol = length(allSeries) + nvar_additional)
for(i in seq_along(allSeries)){
  print(i)
  idseries <- allSeries[i]
  res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", idseries, ".Rdata", sep = "")) 
  load(res_file)

  MAT[i, ] <- as.numeric(model_final$beta)
  which(as.numeric(model_final$beta) !=0)
}
#stop("done")

#MAT <- matrix(NA, nrow = length(bottomSeries), ncol = length(allinputSeries) + nvar_additional)
#for(i in seq_along(bottomSeries)){
#  print(i)
#  idseries <- bottomSeries[i]
#  res_file <- file.path(work.folder, "revisedf", paste("revised_meanf_", idseries, ".Rdata", sep = "")) 
#  load(res_file)
  
#  MAT[i, ] <- as.numeric(model_final$beta)
  #which(as.numeric(model_final$beta) !=0)
#}

library(SparseM)
savepdf(file.path(results.folder, paste("Pmatrix-MEANCOMB2") ), height = 27 * 0.3)
image(as.matrix.csr(MAT), col=c("white","black"))
dev.off()

MATNOTZERO <- (MAT != 0)
res <- apply(MATNOTZERO, 1, sum)
hist(res)

res_sort <- sort(apply(MATNOTZERO, 2, sum), index = T, decreasing = T)

head(res_sort$x, 20)
allinputSeries[head(res_sort$ix, 20)]


image(as.matrix.csr(MAT[, res_sort$ix]))


library(ggplot2)
library(reshape2)
gplot_matrix <- function(X){
  nbrow <- nrow(X)
  Z <- melt(t(X))
  Zp <- Z
  Zp$Var2 <- nbrow + 1 - Zp$Var2
  
  
  p <- ggplot(data = Zp, aes(x = Var1, y = Var2)) + 
    geom_tile(aes(fill = value), colour = "white") + 
    scale_fill_gradient2(name = expression(log(abs(tilde(epsilon)))), low="darkblue", high="darkgreen", guide="colorbar")+
    #scale_fill_gradient2(name = expression(log(abs(tilde(epsilon)))), low="white", high="black", guide="colorbar")+
#    scale_x_continuous(breaks = mybreaks, labels = mylabels) +
    xlab("Hour of the day") + 
    ylab("ID of aggregate series") +
    theme_bw()
  #  geom_vline(xintercept = seq(1, nhours, by = 48), linetype=3) +
  
  p
}  

q <- quantile(as.numeric(MAT), probs = c(0.001, 0.999))
myMAT <- pmin(MAT, q[2])
myMAT <- pmax(myMAT, q[1])

savepdf(file.path(results.folder, paste("Pmatrix-MEANCOMB3") ), height = 27 * 0.3, width = 21 * 0.5)
p <- gplot_matrix(myMAT)
print(p)
dev.off()
#tools::compactPDF(file.path(results.folder, paste("Pmatrix-MEANCOMB3") ))


