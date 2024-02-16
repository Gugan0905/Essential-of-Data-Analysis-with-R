
rm(list=ls())

setwd("D:/Gugan/6th Sem/EDA/Lab/scripts")
data <- read.csv("ClothReview.csv")

library(dplyr)
data=sample_frac(data,0.75)



# install.packages("readr")
library(readr)

#Text mining packages
# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)

corpus = VCorpus(VectorSource(data$Review.Text))
corpus[[1]][1]
data$Recommended.IND[1]

corpus = tm_map(corpus,PlainTextDocument)
corpus = tm_map(corpus,content_transformer(tolower))
corpus[[1]][1]

corpus = tm_map(corpus,removePunctuation)
corpus[[1]][1]

corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))

corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.99)

tSparse = as.data.frame(as.matrix(sparse))
colnames(tSparse) = make.names(colnames(tSparse))
tSparse$recommended = data$Recommended.IND

# install.packages("caTools")
library(caTools)
split = sample.split(tSparse$recommended, SplitRatio = 0.6)
trainSparse = subset(tSparse, split==TRUE)
testSparse = subset(tSparse, split==FALSE)

#library(randomForest)
trainSparse$recommended = as.factor(trainSparse$recommended)
testSparse$recommended= as.factor(testSparse$recommended)

RF_model = randomForest(recommended ~ ., data=trainSparse)
predictRF = predict(RF_model, newdata=testSparse)
#library(caret)
confusionMatrix(table(predictRF,testSparse$recommended))
                
                