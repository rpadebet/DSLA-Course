ap<-AirPassengers

# Description of Time series
str(ap)
class(ap)
attributes(ap)

# Plotting Time series
plot.ts(ap)

# Decomposing time series
apts<-ts(ap,frequency = 12)
apts.decomp<-decompose(apts)

attributes(apts.decomp)

# Decomposed plot
plot(apts.decomp)

# Seasonal Factors
plot(apts.decomp$figure)

# Adjusting the time series
apts.sadj<-apts - apts.decomp$seasonal
plot.ts(apts.sadj,main="Air Passengers Adjusting for Seasonal Factors")

apts.tradj<-apts - apts.decomp$trend
plot.ts(apts.tradj,main="Air Passengers Adjusting for Trend")

aptslog<-log(apts)
aptslog.decomp <- decompose(aptslog)
plot(aptslog.decomp)

aptslog.tradj<-aptslog - aptslog.decomp$trend
plot.ts(aptslog.tradj,main="log Air Passengers Adjusting for Trend")

aptslog.sadj<-aptslog - aptslog.decomp$seasonal
plot.ts(aptslog.sadj,main="log Air Passengers Adjusting for Seasonal Factors")

aptslog.stadj<-aptslog - aptslog.decomp$seasonal-aptslog.decomp$trend
plot.ts(aptslog.stadj,main="log Air Passengers Adjusting for Seasonal & Trend")

acf(aptslog.stadj, na.action = na.pass)

pacf(aptslog.stadj, na.action = na.pass)

Box.test(aptslog.stadj,type = "Ljung-Box")
Box.test(aptslog.stadj)

tseries::adf.test(complete.cases(aptslog.stadj),alternative = "stationary")



## Plots for differencing

par(mfrow=c(3,1),mar = c(2,2,3,0.5))
plot.ts(aptslog,main = "Air Passengers (log transformed)",col = "blue")
plot.ts(diff(aptslog,1),main = "Air Passengers 1st Order (1 Difference to remove trend)", col = "red")
plot.ts(diff(diff(aptslog,1),12),main = "Air Passengers 2nd Order (12 Difference to remove seasonality)", col="green")

par(mfrow=c(2,1),mar = c(2,2,3,0.5) )
plot.ts(apts,main = "Air Passengers",col = "orange")
plot.ts(log(apts),main = "Air Passengers (Log Transformed)", col = "blue")


## ACF

par(mfrow = c(1,1), bg="white",fg="black", col.axis = "darkred")
acf(aptslog, main = "ACF of log(Air Passengers)", col="blue")
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")

par(mfrow = c(1,1), bg="white",fg="black", col.axis = "darkred")
acf(diff(diff(aptslog,1),12), main = "ACF of Differenced log(Air Passengers)", col="brown")

## PACF
par(mfrow = c(1,1), bg="white",fg="black", col.axis = "darkred")
pacf(aptslog, main = "PACF of log(Air Passengers)", col="blue")
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")

par(mfrow = c(1,1), bg="white",fg="black", col.axis = "darkred")
pacf(diff(diff(aptslog,1),12), main = "PACF of Differenced log(Air Passengers)", col="brown")


### Stationary time series
StationSeries <- ts(rnorm(n = 1000))
par(mfrow = c(2,1), bg="white",fg="black", col.axis = "darkred",mar = c(2,2,3,0.5))
plot.ts(StationSeries,main = "White Noise")
hist(StationSeries , main = "Distribution of White Noise")

par(mfrow = c(2,1), bg="white",fg="black", col.axis = "darkred",mar = c(2,2,3,0.5))
acf(StationSeries, main = "ACF of White Noise/Random Walk")
pacf(StationSeries,main = "PACF of White Noise/Random Walk")






# Fitting AR Model and Prediction
statseries<-diff(diff(log(AirPassengers),12),1)
par(mfrow = c(1,1), bg="white",fg="black", col.axis = "darkred")
fit <- arima(statseries, c(1, 0, 0))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(statseries,pred$pred, lty = c(1,3), col="blue", main = "Air Passengers Stationary Series Prediction")




# Fitting Model and Prediction
par(mfrow = c(1,1), bg="white",fg="black", col.axis = "darkred")
fit <- arima(log(AirPassengers), c(1, 1, 0),seasonal = list(order = c(1, 1, 0), period = 12))
fit2 <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3), col="blue", main = "Air Passengers Prediction")


# Fitting Model using auto.ARIMA
library(forecast)
findbest <- auto.arima(AirPassengers)
findbest
predfore <-forecast(findbest,h=120)
plot(predfore)


## ACF PACF of diff Air Passengers
par(mfrow = c(1,1), bg="white",fg="black", col.axis = "darkred")
acf(diff(aptslog,1), main = "ACF of Differenced log(Air Passengers)", col="brown")
pacf(diff(aptslog,1), main = "PACF of Differenced log(Air Passengers)", col="brown")
