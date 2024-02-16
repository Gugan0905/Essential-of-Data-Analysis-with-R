rm(list=ls())

data <- read.csv("Train.csv",stringsAsFactors = T)
logreg <- glm(formula = data$creditScore ~.,family='binomial', data = data)
summary(logreg)

logitrain <- predict(logreg, type='response') # type='response' gives probability otherwise the log-odds by default
plot(logitrain)
tapply(logitrain,data$creditScore,mean)


TEST_data <- read.csv("Test.csv",stringsAsFactors = T)
logitest <- predict(logreg, newdata = TEST_data, type='response')
plot(logitest)
tapply(logitest,TEST_data$creditScore,mean)
TEST_data[logitest <=0.7, "LogiTest"]="bad"
TEST_data[logitest >0.7, "LogiTest"]="good"

#install.packages("caret")
library(caret)
confusionMatrix(table(TEST_data[,4],TEST_data[,5]),positive='good')

###################
#For crashTest dataset
###################
rm(list=ls())

data <- read.csv("crashTest_TRAIN.csv",stringsAsFactors = T)
logreg <- glm(formula = data$CarType ~.,family='binomial', data = data)
summary(logreg)

logitrain <- predict(logreg, type='response') # type='response' gives probability otherwise the log-odds by default
plot(logitrain)
tapply(logitrain,data$CarType,mean)


TEST_data <- read.csv("crashTest_TEST.csv",stringsAsFactors = T)
logitest <- predict(logreg, newdata = TEST_data, type='response')
plot(logitest)
tapply(logitest,TEST_data$CarType,mean)
TEST_data[logitest <=0.7, "LogiTest"]="Hatchback"
TEST_data[logitest >0.7, "LogiTest"]="SUV"

#install.packages("caret")
library(caret)
confusionMatrix(table(TEST_data[,7],TEST_data[,8]),positive='Hatchback')
