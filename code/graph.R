
########## Bulding the tree + plotting
unique(infoDT$NUTS1)
"---" "UKD" "UKF" "UKG" "UKI" "UKJ" "UKK" "UKL" "UKM"

myset <- setdiff(unique(infoDT$NUTS1), c("---", "UKD"))
for(id in myset){
  print(id)
  myDT <- dplyr::filter(infoDT, NUTS1 == id)
  c <- data.frame(rbind(cbind(id,myDT$NUTS2), cbind(myDT$NUTS2, myDT$NUTS3), cbind(myDT$NUTS3, myDT$NUTS4) ))
  
  g <- graph.data.frame(c)
  g <- simplify(g, remove.loops = F)
  plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.label.cex = 0.4, vertex.size = 1)
}
dev.off()



########## USEFUL FUNCTIONS TO COMPUTE S_a and to identify the children to apply permutations

g1 <- graph( edges=c("1","2", "1","3", "1", "4", "2", "8", "2", "M9"), directed=T)
plot(g1, layout= layout.reingold.tilford(g1, root=1))
reachable <- which(shortest.paths(g1, "2", mode="out") != Inf)
terminal.nodes <- reachable[which(degree(g1, reachable, mode="out") == 0)]
V(g1)[terminal.nodes]

# Useful to compute S_a

# all with 0 are bottom (leaf) nodes
degree(g1, V(g1), "out")

degree(g1, 1, "out")
subcomponent(g1, "2", "out")