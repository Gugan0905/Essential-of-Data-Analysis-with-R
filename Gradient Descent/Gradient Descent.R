setwd('D:/Gugan/6th Sem/EDA/Lab/scripts')
######
#SAMPLE CODE
######
library(dplyr)
rm(list=ls())
data <- mtcars
data <-sample_n(data,8)
GRADIENT.DESCENT <- function(y, x, alpha, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    m_new <- m - alpha * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - alpha * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m, "No of iterations:", iterations,"MSE:", MSE_new))
    }
    iterations = iterations + 1
    if(iterations >= max_iter) { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m, "No of iterations:", iterations,"MSE:", MSE_new))
    }
  }
}

GRADIENT.DESCENT(data$mpg, data$wt, 0.25, 0.02, length(data$mpg), 4000)
slr <- lm(mpg ~ wt, data = mtcars)
slr$coef
mpg_p <- predict(slr)
sqerr <- (data$mpg - mpg_p)^2
MSE.SLR <- sum(sqerr)/length(data$mpg)

##########################
##MPG vs WT
#SLR MSE = 8.697560
#LR, Num iterations, GD MSE
#0.001, 2000, 1076.62
#0.001, 2500, 1143.28
#0.001, 3000, 1138.98
#0.01, 2000, 1039.21
#0.01, 2500, 1029.55
#0.01, 3000, 1074.38
#0.02, 2000, 1029.45
#0.02, 2500, 990.05
#0.02, 3000, 920.75
#0.02, 4000, 1113.95

cor(data$mpg, data$wt)   #-0.8676594
cor(data$mpg, data$qsec) #0.418684 
cor(data$mpg, data$disp) #-0.8475514
cor(data$wt, data$disp) #0.8879799
#######
##mpg vs DISP
slr <- lm(mpg ~ disp, data = data)
slr$coef
dis_p <- predict(slr)
sqerr <- (data$mpg - dis_p)^2
MSE.SLR <- sum(sqerr)/length(data$mpg)
MSE.SLR

GRADIENT.DESCENT(data$mpg, data$disp, 0.000001, 0.1, length(data$mpg), 25000000)



