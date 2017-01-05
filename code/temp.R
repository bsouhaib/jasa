stop("done")
############################
pdf(paste("NUTS-", myregion, ".pdf", sep= ""))
#myDT <- dplyr::filter(subInfoDT, IDMETER %in% finalmeters)
myDT <- x
c <- data.frame(rbind(cbind(myregion,myDT$NUTS2), cbind(myDT$NUTS2, myDT$NUTS3), cbind(myDT$NUTS3, myDT$NUTS4) ))
g <- graph.data.frame(c)
g <- simplify(g, remove.loops = F)
plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.label.cex = 0.4, vertex.size = 1, vertex.label.dist = .2)
dev.off()

#print(dim(subInfoDT %>% filter(IDMETER %in% finalmeters)))
#print(as.data.frame(subInfoDT %>% filter(IDMETER %in% finalmeters) %>% count(NUTS1, NUTS4) %>% arrange(n)))

print(dim(x))
print(as.data.frame(x %>% count(NUTS1, NUTS4) %>% arrange(n)))


pdf(paste("DEMO-", myregion, ".pdf", sep= ""))
c <- data.frame(rbind(cbind("DEMO",myDT$DEMO1), cbind(myDT$DEMO1, myDT$DEMO2), cbind(myDT$DEMO2, myDT$DEMO3) ))
g <- graph.data.frame(c)
g <- simplify(g, remove.loops = F)
plot(g, layout = layout.reingold.tilford(g, root=1, circular=T), vertex.label.cex = 0.4, vertex.size = 1, vertex.label.dist = .2)
dev.off()
#print(as.data.frame(subInfoDT %>% filter(IDMETER %in% finalmeters) %>% count(DEMO1, DEMO3) %>% arrange(n)))

#print(as.data.frame(x %>% count(DEMO1, DEMO3) %>% arrange(n)))


# scp -i "bsouhaib-key-pair-sydney.pem" ubuntu@ec2-13-54-209-237.ap-southeast-2.compute.amazonaws.com:/home/rstudio/codemeters/code/Rplots.pdf .


missingPerDay <- function(meterseries){
  mymat <- matrix(meterseries, ncol = 48, byrow = T)
  ndays <- nrow(mymat)
  vec <- apply(t(apply(mymat, 1, is.na)), 1, sum)
  sort(vec[which(vec != 0)])
}