rm(list=ls())

# Month-wise highest price/gm of Gold in India from Jan-2018 to Dec-2020
vec=c(3016, 3044, 3041, 3121, 3111, 3043, 2977, 3036, 3051, 3191, 3016, 3164, 3321, 3338, 3170, 3194, 3212, 3420, 3465, 3866, 3774, 3858, 3807, 3922, 4105, 4141, 4383, 4587, 4656, 4864, 5373, 5179, 5068, 5071, 4814, 5024)
data <- ts(vec, start=c(2018,1), end=c(2020,12), frequency=12)

start(data)
end(data)
frequency(data)
cycle(data)
summary(data)

# plotting the series
plot(data)

#regression
abline(reg=lm(data~time(data)), col="red")

#month-wise (range and mean) plot 
monthplot(data)

# year-wise mean plot
plot(aggregate(data,FUN=mean))

# month-wise box plot
boxplot(data~cycle(data))

# install.packages("forecast")
library(forecast)
seasonplot(data)

# Autocorrelation and Partial Autocorrelation plots
acf(data)
pacf(data, lag=length(data),pl=TRUE)

# ARIMA
fit <- arima(data, order=c(3, 2, 2))
accuracy(fit)
newdata <- forecast(fit, 4)
plot(newdata)

# Auto ARIMA 
fit <- auto.arima(data)
newdata <- forecast(fit, 4)
plot(newdata)

################################################
#For Air Passengers

rm(list=ls())
data(AirPassengers)
data<-AirPassengers

class(data)
start(data)
end(data)
frequency(data)
cycle(data)
summary(data)

# plotting the series
plot(data)

#regression
abline(reg=lm(data~time(data)), col="red")

#month-wise (range and mean) plot 
monthplot(data)

# year-wise mean plot
plot(aggregate(data,FUN=mean))

# month-wise box plot
boxplot(data~cycle(data))

library(forecast)
seasonplot(data)

# Autocorrelation and Partial Autocorrelation plots
acf(data)
pacf(data, lag=length(data),pl=TRUE)

# ARIMA
fit <- arima(data, order=c(3, 2, 2))
accuracy(fit)        
newdata <- forecast(fit, 4)
plot(newdata)

# Auto ARIMA 
fit <- auto.arima(data)
newdata <- forecast(fit, 4)
plot(newdata)

################################################
#For Petrol Price

rm(list=ls())


data2 <- read.csv("PetrolPriceChennai.csv")
data3 <- data2$Closing
data4 <-ts(data3, start=c(2018,1), end=c(2020,12), frequency=12)

start(data4)
end(data4)
frequency(data4)
cycle(data4)
summary(data4)

# plotting the series
plot(data4)

#regression
abline(reg=lm(data4~time(data4)), col="red")

#month-wise (range and mean) plot 
monthplot(data4)

# year-wise mean plot
plot(aggregate(data4,FUN=mean))

# month-wise box plot
boxplot(data4~cycle(data4))

# install.packages("forecast")
library(forecast)
seasonplot(data4)

# Autocorrelation and Partial Autocorrelation plots
acf(data4)
pacf(data4, lag=length(data4),pl=TRUE)

# ARIMA
fit <- arima(data4, order=c(3, 2, 2))
accuracy(fit)
newdata <- forecast(fit, 4)
plot(newdata)

# Auto ARIMA 
fit <- auto.arima(data4)
newdata <- forecast(fit, 4)
plot(newdata)
