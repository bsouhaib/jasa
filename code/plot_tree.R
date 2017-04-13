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

g <- itree
v_names <- names(V(g))
v_names[seq(2, length(v_names))] <- substr(v_names[seq(2, length(v_names))], 4, 7)
v_names[seq(56, length(v_names))] <- ""
g <- set.vertex.attribute(g, "name", value=v_names)

savepdf(file.path(results.folder, paste("hierarchy-plot", sep = "") ))

#plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.size=0, edge.arrow.size=0, vertex.label.cex = .7, 
#     vertex.label.dist=.3, vertex.label.degree = .30)

myvsize <- c(apply(Sagg, 1, sum), rep(0, ncol(Sagg)))
#plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.size=myvsize/90, edge.arrow.size=0, vertex.label.cex = .7)

plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.size=myvsize/90, edge.arrow.size=0, vertex.label= NA)

endpdf()


