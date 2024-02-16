rm(list=ls()) 

setwd("D:\\Gugan\\6th Sem\\EDA\\Lab\\scripts") 

data <- read.csv("Credit.csv",stringsAsFactors = T) 

data$Cpur<-as.integer(data$Cpur) 

data$Prop<-as.integer(data$Prop) 

data[,-6] <- scale(data[,-6]) 

#install.packages("dplyr") 

library(dplyr) 

data_TRAIN <-sample_n(data,900) 

data_TEST <-setdiff(data,data_TRAIN) 



# install.packages("class") # for K-NN 

library(class) 

knnpredict <- knn(train=data_TRAIN[,-6],test=data_TEST[,-6],cl=data_TRAIN$creditScore, k=5) 



# install.packages("caret") # Classification and Regression Training  

library(caret) 

confusionMatrix(table(knnpredict,data_TEST$creditScore),positive='good') 

#################################################
#Parkinsons for 80:20 split
################################################
rm(list=ls())
data <- read.csv("Parkinsons.csv",stringsAsFactors = T)
#install.packages("dplyr")
library(dplyr)
data_TRAIN <-sample_n(data,156)
data_TEST <-setdiff(data,data_TRAIN)

# install.packages("class") # for K-NN
library(class)
knnpredict <- knn(train=data_TRAIN[,-6],test=data_TEST[,-6],cl=data_TRAIN$status, k=3)

# install.packages("caret") # Classification and Regression Training 
library(caret)
confusionMatrix(table(knnpredict,data_TEST$status),positive='1')


#################################################
#Parkinsons for 60:40 split
################################################
rm(list=ls())
data <- read.csv("Parkinsons.csv",stringsAsFactors = T)
#install.packages("dplyr")
library(dplyr)
data_TRAIN <-sample_n(data,117)
data_TEST <-setdiff(data,data_TRAIN)

# install.packages("class") # for K-NN
library(class)
knnpredict <- knn(train=data_TRAIN[,-6],test=data_TEST[,-6],cl=data_TRAIN$status, k=9)

# install.packages("caret") # Classification and Regression Training 
library(caret)
confusionMatrix(table(knnpredict,data_TEST$status),positive='1')
