rm(list=ls())
setwd("D:\\Gugan\\6th Sem\\EDA\\Lab\\scripts")
# Importing the dataset
dataset = read.csv('social.csv')

# Taking columns 3-5
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting SVM to the Training set
#install.packages('e1071')
library(e1071)

classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'sigmoid')
par(mar=c(1,1,1,1))
dev.new(width=10, height=10)
plot(classifier,training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])


library(caret)
confusionMatrix(table(y_pred,test_set$Purchased))


#########################
#Logistic Regression
#############

logreg <- glm(formula = training_set$Purchased ~.,family='binomial', data = training_set)
summary(logreg)

logitrain <- predict(logreg, type='response') # type='response' gives probability otherwise the log-odds by default
plot(logitrain)
tapply(logitrain,training_set$Purchased,mean)


TEST_data <- test_set
logitest <- predict(logreg, newdata = TEST_data, type='response')
plot(logitest)
tapply(logitest,TEST_data$Purchased,mean)
TEST_data[logitest <=0.7, "LogiTest"]=0
TEST_data[logitest >0.7, "LogiTest"]=1

#install.packages("caret")
library(caret)
confusionMatrix(table(TEST_data[,3],TEST_data[,4]),positive='1')

#########################
#Trying with KNN 
#############


# install.packages("class") # for K-NN
library(class)
knnpredict <- knn(train=training_set[,-4],test=test_set[,-4],cl=training_set$Purchased, k=5)

# install.packages("caret") # Classification and Regression Training 
library(caret)
confusionMatrix(table(knnpredict,test_set$Purchased),positive='1')

