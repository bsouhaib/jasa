# x <- msts(taylor, seasonal.periods=c(48,336), start=2000+22/52)
# f1 <- forecast(x, h = 270)

# x <- msts(taylor, seasonal.periods = c(48, 336))
# f2 <- forecast(x, h = 270)

# taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
# f3 <- forecast(taylor.lm, data.frame(fourier(taylor, K = c(3, 3), h = 270)))

# x <- msts(y, seasonal.periods = c(48, 336))
# fit <- Arima(y, order=c(2,0,1), xreg=fourier(y, K=4)) or fit <- auto.arima(x, seasonal=FALSE, xreg=fourier(x, K= c(2, 2) ))
# e <- resid(fit)
# plot(forecast(fit, h=48, xreg=fourier(x, K= c(2, 2), h = 48)), include = 48 *7)
# plot(forecast(fit, h=2*m, xreg=fourier(y, K=4, h=2*m)))
# 

# x <- msts(y, seasonal.periods = c(48, 336))
# fcast <- forecast(x, h = 48 * 6)
