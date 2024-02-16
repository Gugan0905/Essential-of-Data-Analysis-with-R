rm(list=ls())
setwd("D:\\Gugan\\6th Sem\\EDA\\Lab\\scripts")
data <- read.csv("USArrests.csv",row.names=1)
df <- scale(data)
dissim <- dist(df, method = 'euclidean')
hierClust <- hclust(dissim, method = 'complete')
dev.new(width=10, height=10)
plot(hierClust)
cluster <- cutree(hierClust, k = 4)

# install.packages("clValid")
library(clValid)
dunn(dissim, cluster)

#plot(hclust_avg)
rect.hclust(hierClust, k = 4, border = 2:4)
abline(h = 4, col = 'red')


####################
#IRIS DATASET
rm(list=ls())
setwd("D:\\Gugan\\6th Sem\\EDA\\Lab\\scripts")
data <- read.csv("iris.csv",row.names=1)
df <- scale(data)
dissim <- dist(df, method = 'euclidean')
hierClust <- hclust(dissim, method = 'single')
dev.new(width=10, height=10)
plot(hierClust)
cluster <- cutree(hierClust, k = 5)

# install.packages("clValid")
library(clValid)
dunn(dissim, cluster)

#plot(hclust_avg)
rect.hclust(hierClust, k = 5, border = 2:4)
abline(h = 0.85, col = 'red')
