###################################
#For Mtcars dataset
###################################

rm(list=ls())
data <- mtcars
library(dplyr)
data <- sample_n(data,15)

# install.packages("ggplot2") 
library(ggplot2)         
ggplot(data,aes(x=wt,y=mpg))+geom_point() #To plot - wt/gear
cor.test(data$wt,data$mpg)   # to find the correlation value

# simple linear regression
slr = lm(mpg~wt, data) 
summary(slr)
plot(slr$resid)  # Residual plot
qqnorm(slr$resid)   #Q-Q Plot

# Multiple linear regression
mlr = lm(mpg~wt+gear, data) 
summary(mlr)
plot(mlr$resid)  # Residual plot
qqnorm(mlr$resid)   #Q-Q Plot


###################################
#For Credit worthiness dataset
###################################

rm(list=ls())
data <- read.csv("CreditWorthiness.csv")
library(dplyr)
data <- sample_n(data,15)

# install.packages("ggplot2") 
library("ggplot2")         
ggplot(data,aes(x=Cdur,y=Camt))+geom_point() #To plot - wt/gear
cor.test(data$Cdur,data$Camt)   # to find the correlation value

# simple linear regression
slr = lm(Camt~Cdur, data) 
summary(slr)
plot(slr$resid)  # Residual plot
qqnorm(slr$resid)   #Q-Q Plot

# Multiple linear regression
mlr = lm(Camt~Cdur+InRate, data) 
summary(mlr)
plot(mlr$resid)  # Residual plot
qqnorm(mlr$resid)   #Q-Q Plot

