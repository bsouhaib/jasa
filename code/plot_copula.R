N <- nrow(U); 

allq <- seq(0.01, 0.99, by = 0.01)
B <- 100
allres <- matrix(NA, nrow = B, ncol = length(allq) )
for(b in seq(B)){
  print(b)
  res <- NULL
  Ub <- U[sample(N, replace = T), ]
  for(q in allq){
    if(q <= 0.5){
      v <- sum(Ub[, 1] <= q & Ub[, 2] <= q)/(N * q)
    }else{
      v <- sum(Ub[, 1] > q & Ub[, 2] > q)/(N * (1 - q) )
    }
    res <- c(res, v)
  }
  allres[b, ] <- res
}

plot(allq, res)
sd <- apply(allres, 2, sd)
u <- apply(allres, 2, mean)

matplot(cbind(u, u + 2 * sd, u - 2 * sd), type = 'l')
