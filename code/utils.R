
quantile_loss <- function(y, f, tau){
  mean(tau*(y - f)*((y - f) >= 0) - (1-tau)*(y - f)*((y - f) < 0))
}

check_function <- function(y, f, tau){
  tau*(y - f)*((y - f) >= 0) - (1-tau)*(y - f)*((y - f) < 0)
}


makeSuperposed<-function(datatable,maxy,colorvar,maintitle=NULL,xlab,ylab,do.plot=TRUE, densities = NULL)
{
  if(length(colorvar)!=ncol(datatable)-1)
    stop("Error color !")
  
  if(!is.null(densities) && length(densities) != ncol(datatable)-1 ){
    stop("Error densities ! ")
  }
  
  #ylim<-c(0,1.1*max(maxy,apply(datatable[,-1],1,sum)))
  ylim<-c(0,maxy)
  xx <- c(datatable[,1], rev(datatable[,1]))
  
  if(do.plot){
    plot(x=datatable[,1], y=datatable[,2], ylim=ylim,type='l',ylab=ylab, xlab=xlab, main=maintitle)
    #		plot(x=datatable[,1], y=datatable[,2], ylim=ylim,type='l',ylab=ylab, xlab=xlab, main=maintitle)
    
  }
  
  a<-datatable[,2]
  yysrc2 <- c(rep(0, nrow(datatable)), rev(a))
  
  allvar <- seq(2,ncol(datatable))
  for(variable in allvar)
  {
    id <- which(variable==allvar)
    
    mydensity <- NULL
    if(!is.null(densities)){
      mydensity <- densities[id]
      if(is.na(mydensity))
        mydensity <- NULL
    }
    
    polygon(xx, yysrc2, col=colorvar[variable-1],border=NA, density = mydensity)
    if(variable != ncol(datatable))
    {
      b<-rev(apply(datatable[,2:(variable+1),drop=F],1,sum))
      yysrc2 <- c(a,b)
      a<-rev(b)
    }
  }
  
}

########
plotQF <- function(quantilef, obs, alphas, id, only.future = FALSE, ...)
{
  qf <- quantilef[alphas, id]
  medianf <- 	quantilef["50%", id]
  future <- obs[id]
  
  # Plotting
  matplot(id, cbind(future, t(qf)), type = "n", ...)
  if(!only.future){
    x <- seq(ncol(qf))
    xx <- c(x, rev(x))
    
    nbrow <- nrow(qf)
    colorvar <- c("grey", "lightblue", "grey")
    idcol <- 1
    for(row in seq(nbrow, 2, by =-1)){
      yy <- c(qf[row, ], rev(qf[row - 1,]))         
      polygon(xx, yy, col=colorvar[idcol],border=NA)
      idcol <- idcol +1
    }
  }
  points(id, future, pch = 20)
  if(!only.future){
    lines(medianf)
  }
}


allqs <- function(qf, obs, alphas){
  allqs <- NULL
  for(id in seq_along(alphas)){
    alpha <- alphas[id]
    #print(alpha)
    qs <- quantileScore(obs, qf[id, ], alpha, breaks = c(-10, union(quantile(qf[id, ]), quantile(qf[id, ])))   )$qs.orig
    allqs <- c(allqs, qs)
    
  }
  allqs
}

