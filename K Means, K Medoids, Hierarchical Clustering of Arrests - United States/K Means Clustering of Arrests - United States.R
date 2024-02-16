rm(list=ls())
#library(dplyr)
setwd("D:\\Gugan\\6th Sem\\EDA\\Lab\\scripts")
data <- read.csv("USArrests.csv",row.names=1)
#df <- sample_n(data, 13) #for 25%
df <- scale(data)

set.seed(112)
fit<- kmeans(df,3)
library("factoextra")
fviz_cluster(fit, data = df, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())
fit$size
fit$withinss
fit$tot.withinss  # Within Cluster Sum of Squares (WCSS)
Kmax <- 15
WCSS <- rep(0,Kmax)
nClust <- list()
for (i in 1:Kmax){
  fit<- kmeans(df,i)
  WCSS[i] <- fit$tot.withinss
  nClust[[i]] <- fit$size
}
dev.new(width=10, height=10)
plot(1:Kmax,WCSS,type="b",pch=19)
# install.packages("factoextra")
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")

library(cluster) 
silhouette_score <- function(k){ 
  km <- pam(df, k, metric = "manhattan") 
  ss <- silhouette(km$cluster, dist(df)) mean(ss[, 3]) } 
k <- 2:10 
avg_sil <- sapply(k, silhouette_score) 
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# install.packages("cluster")
library(cluster)
fit <- pam(df, 3, metric = "manhattan") # K-Medoids
print(fit)

fviz_nbclust(df, pam, method = "silhouette")
