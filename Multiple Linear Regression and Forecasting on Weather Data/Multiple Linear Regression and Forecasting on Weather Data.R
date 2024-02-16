rm(list=ls())
a <- read.csv("weatherHistory2016.csv") 
# Multiple linear regression
mlr=lm(Temperature..C.~Apparent.Temperature..C.+Humidity+Wind.Speed..km.h., a) 
summary(mlr)
qqnorm(mlr$resid)   #Q-Q Plot
# Time-series analysis
data <- ts(a$Temperature..C., start=as.Date("2016-01-01"), end=as.Date("2016-12-31"), frequency=24)
frequency(data)
summary(data)
# plotting the series
plot(data)
# day-wise mean plot
plot(aggregate(data,FUN=mean))
# hour-wise box plot
boxplot(data~cycle(data))
# install.packages("forecast")
library(forecast)
# Autocorrelation and Partial Autocorrelation plots
acf(data)
# Time-series forecasting (Auto ARIMA) 
fit <- auto.arima(data)
accuracy(fit)
newdata <- forecast(fit, 240)
plot(newdata)


###############################
#Ex no 3 assignment

b= read.csv("weather.csv")
b$date <- as.Date(b$Formatted.Date,format="%Y-%m-%d")
c <- b[b$date>=as.Date(as.character("2007-01-01"),format="%Y-%m-%d") &
         b$date<=as.Date(as.character("2007-12-31"),format="%Y-%m-%d"),]

mlr=lm(Humidity~Temperature..C.+Wind.Speed..km.h.+Pressure..millibars., c)
summary(mlr)
qqnorm(mlr$resid)   #Q-Q Plot

mlr2=lm(Pressure..millibars.~Temperature..C.+Wind.Speed..km.h.+Humidity, c)
summary(mlr2)
qqnorm(mlr2$resid)



data2 <- ts(c$Humidity, start=as.Date("2008-01-01"), end=as.Date("2008-01-20"), frequency=24)
frequency(data2)
summary(data2)
plot(data2)
plot(aggregate(data2,FUN=mean))
boxplot(data2~cycle(data2))
library(forecast)
acf(data2)
fit2 <- auto.arima(data2)
accuracy(fit2)
newdata2 <- forecast(fit2, 240)
plot(newdata2)


data3 <- ts(c$Pressure..millibars., start=as.Date("2008-01-01"), end=as.Date("2008-01-31"), frequency=24)
frequency(data3)
summary(data3)
plot(data3)
plot(aggregate(data3,FUN=mean))
boxplot(data3~cycle(data3))
library(forecast)
acf(data3)
fit3 <- auto.arima(data3)
accuracy(fit3)
newdata3 <- forecast(fit3, 240)
plot(newdata3)
