rm(list = ls())

source("config_paths.R")
source("config_general.R")
source("config_splitting.R")
source("jasa_utils.R")
source("utils.R")

library(forecast)
load(file.path(work.folder, "myinfo.Rdata"))

iseries <- 1
idseries <- aggSeries[iseries]
load(file.path(aggseries.folder, paste("series-", idseries, ".Rdata", sep = "")))

#iseries <- 1
#idseries <- bottomSeries[iseries]
#load(file.path(mymeters.folder, paste("mymeter-", idseries, ".Rdata", sep = "")))


y <- demand[learn$id]
T <- length(y)

m_1 <- 48
m_2 <- 336

# initialization
a <- 1/336 * (mean(y[seq(336)]) - mean(y[336 + seq(336)]))
b <- mean(diff(y[seq(336)]))
T_0 <- (a+b)/2
l_start <- mean(y[seq(2 * 336)]) - 336.5 * T_0

# days
nb_obs <- 7 * m_1
indices <- seq(nb_obs)
smoothed_line <- ma(y[indices], m_1)
#indices <- seq(m_1/2 + 1, nb_obs - m_1/2)
x <- y[indices] - smoothed_line[indices]
mat <- matrix(x, ncol = 48, byrow = T)
D <- apply(mat, 2, mean, na.rm = T)
D <- D - mean(D)

# weeks
nb_weeks <- 4
indices <- seq(nb_weeks * m_2)
smoothed_line <- ma(y[indices], m_2)
x <- y[indices] - smoothed_line[indices] - rep(D, nb_weeks * 7)
mat <- matrix(x, ncol = 336, byrow = T)
W <- apply(mat, 2, mean, na.rm = T)
W <- W - mean(W)

e_0 <- rep(0, m_2)
l_0 <- rep(l_start, m_2)
d_0 <- rep(D, 7)
w_0 <- W


iterate <- function(theta, y, e_0, l_0, d_0, w_0){
  phi   <- theta[1]
  alpha <- theta[2]
  delta <- theta[3]
  omega <- theta[4]

  n <- length(y)
  yhat <- e <- l <- d <- w <- numeric(n) + NA
  yhat_local_0 <- numeric(m_2 * 2) + NA
  e_local_0 <- e_0 
  l_local_0 <- l_0
  d_local_0 <- d_0
  w_local_0 <- w_0
  
  z <- numeric(m_2 * 2) + NA
  z[seq(m_2 + 1, 2 * m_2)] <- y[seq(m_2)]
  for(i in seq(m_2 + 1, 2 * m_2)){
    yhat_local_0[i]   <- l_local_0[i - 1] + d_local_0[i - m_1] + w_local_0[i - m_2] + phi * e_local_0[i - 1]
    e_local_0[i] <- z[i] - (l_local_0[i - 1] + d_local_0[i - m_1] + w_local_0[i - m_2]) # added - m_2 for y
    l_local_0[i] <- l_local_0[i - 1] + alpha * e_local_0[i]
    d_local_0[i] <- d_local_0[i - m_1] + delta * e_local_0[i]
    w_local_0[i] <- w_local_0[i - m_2] + omega * e_local_0[i]
    browser()
  }
  yhat[seq(m_2)]   <- yhat_local_0[m_2 + seq(1, m_2)]
  e[seq(m_2)] <- e_local_0[m_2 + seq(1, m_2)]
  l[seq(m_2)] <- l_local_0[m_2 + seq(1, m_2)]
  d[seq(m_2)] <- d_local_0[m_2 + seq(1, m_2)]
  w[seq(m_2)] <- w_local_0[m_2 + seq(1, m_2)]
  
  for(i in seq(m_2 + 1, n)){
    yhat[i]   <- l[i - 1] + d[i - m_1] + w[i - m_2] + phi * e[i - 1]
    e[i] <- y[i] - (l[i - 1] + d[i - m_1] + w[i - m_2])
    l[i] <- l[i - 1] + alpha * e[i]
    d[i] <- d[i - m_1] + delta * e[i]
    w[i] <- w[i - m_2] + omega * e[i]
  }
  list(yhat = yhat, e = e, l = l, d = d, w = w)
}

my_theta <- c(0.9, 0.1, 0.3, 0.5)
res <- iterate(my_theta, y, e_0, l_0, d_0, w_0)
mean((y - res$yhat)^2)



func(c(0, 0, 0))
func(c(0.5, 0.1, 0.2))

N <- 1000
THETA <- cbind(runif(N), runif(N), runif(N))
E <- sapply(seq(nrow(THETA)), function(i){ 
  print(i)
  func(THETA[i, ])
})

#ids_best <- sort(E, index = T)$ix[1:10]
#lapply(ids_best, function(id){
#  res <- optim(THETA[id, ], fn = func, method = "L-BFGS-B", lower = 0, upper = 1)
#  print(res$value)
#})

id <- sort(E, index = T)$ix[1]
res <- optim(THETA[id, ], fn = func, method = "L-BFGS-B", lower = 0, upper = 1)


alpha <- res$par[1]
delta <- res$par[2]
omega <- res$par[3]

for(t in 337:T){
  e[t] <- y[t] - (l[t-1] + d[t - m_1] + w[t - m_2])
  l[t] <- l[t - 1] + alpha * e[t]
  d[t] <- d[t - m_1] + delta * e[t]
  w[t] <- w[t - m_2] + omega * e[t]
}


v <- l + d + w
residuals <- y - v
residuals <- tail(residuals, - 336)
# plot(residuals, type = "h")
model <- auto.arima(residuals)

plot.ts(y[15000:16000])
lines(v[15000:16000], col = "red")


