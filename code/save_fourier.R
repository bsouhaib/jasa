
load(file.path(aggseries.folder, paste("series-", aggSeries[1], ".Rdata", sep = "")))

residuals.folder <- file.path(work.folder , "residuals")
resid_file <- file.path(residuals.folder, "TBATS", paste("residuals_", aggSeries[1], "_", "TBATS", "_", 1, ".Rdata", sep = "")) 
load(resid_file)

# run auto.arima for the first day, and use Arima(y, model = previous.model) for subsequent days with refitting every ???

n <- 48 * 30 * 4
y <- msts(head(demand, n), seasonal.periods = c(48, 336))


nplot <- 48 * 7 *2
plot.ts(head(y, nplot))
lines(head(demand, nplot) - head(e_residuals, nplot), col = "blue")


p1 <- 48
p2 <- 336
max_k1 <- 24
max_k2 <- 168
X <- fourier(y, K = c(max_k1, max_k2))
allvar <- colnames(X)


model <- glmnet(y = y, x = X, alpha = 1, lambda = 0)
yhat <- as.numeric(predict(model, X))
e <- y - yhat
model_arima <- auto.arima(e)
plot.ts(as.numeric(y))

yhat_bis <- as.numeric(yhat + fitted(model_arima))
e_residuals_bis <- y - yhat_bis
lines(yhat_bis, col = "purple")

lines(yhat, col = "red")

y <- y + rnorm(y, sd = sd(y)/100)
plot(lm(log(y)~X))

res_ridge <- cv.glmnet(X, as.numeric(y), nfolds = 3, alpha = 0)
res_lasso <- cv.glmnet(X, as.numeric(y), nfolds = 3, alpha = 1)
  
model <- glmnet(y = y, x = X, alpha = 0, lambda = res_ridge$lambda[which.min(res_ridge$cvm)]); plot(model)
model <- glmnet(y = y, x = X, alpha = 1, lambda = res_lasso$lambda[which.min(res_lasso$cvm)]); plot(model)

model <- glmnet(y = y, x = X, alpha = .9, lambda = res$lambda[which.min(res$cvm)])
yhat <- as.numeric(predict(model, X))
plot.ts(head(y, 48 * 7 *2))
lines(head(yhat, 48 * 7 *2), col = "red")

plot(res)

mat <- matrix(NA, nrow = 24, ncol = 168)
for(k1 in seq(1, 24)){
  print(k1)
  for(k2 in seq(1, 168)){
    #print(k2)
    #X <- fourier(taylor, K = c(k1, k2))
    
    myvar <- c(paste("C", seq(k1), "-", p1, sep = ""),
               paste("S", seq(k1), "-", p1, sep = ""),
               paste("S", seq(k2), "-", p2, sep = ""),
               paste("S", seq(k2), "-", p2, sep = ""))
    
    idvar <- match(myvar, allvar)
    idna <- which(is.na(idvar))
    if(length(idna)!=0){
      idvar <- idvar[-idna]
    }
    #stopifnot(all(!is.na(idvar)))
    
    model <- lm(y~X[, idvar])
    mat[k1, k2] <- AIC(model)
    #print(AIC(model))
  }
}

kbest <- arrayInd(which.min(mat), .dim = dim(mat))
k1best <- kbest[1]
k2best <- kbest[2]

X <- fourier(y, K = c(k1best, k2best))
model <- lm(y~X)
plot.ts(as.numeric(y))
lines(fitted(model), col = "red")


plot.ts(head(y, 48 * 7 *2))
lines(head(fitted(model), 48 * 7 *2), col = "red")


vec <- numeric(length(bottomSeries))
par(mfrow = c(1, 2))
for(i in seq_along(bottomSeries)){
  print(i)
  load(file.path(mymeters.folder, paste("mymeter-", bottomSeries[i], ".Rdata", sep = "")))
  
  vec[i] <- 100*(length(unique(demand))/length(demand))
  
  v <- demand + rnorm(length(demand), sd = sd(demand)/100) #  NO NEGATIVE VALUES
  
  D <- matrix(demand, ncol = 48, byrow = T)
  V <- matrix(v, ncol = 48, byrow = T)
  for(h in seq(48)){
    plot(ecdf(D[, h]))
    plot(ecdf(V[, h]), col = "red")
    browser()
  }

}



plot.ts(head(v, 48 *7))
lines(head(demand, 48 *7), col = "red")

D <- matrix(demand, ncol = 48, byrow = T)
V <- matrix(v, ncol = 48, byrow = T)
for(h in seq(48)){
  plot(ecdf(D[, h]))
  plot(ecdf(V[, h]), col = "red")
  browser()
}
