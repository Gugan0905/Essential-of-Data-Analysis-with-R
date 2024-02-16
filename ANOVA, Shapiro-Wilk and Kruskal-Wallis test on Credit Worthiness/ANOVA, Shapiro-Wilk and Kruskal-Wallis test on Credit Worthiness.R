rm(list=ls())
data <- data.frame(sale.count=c(40,60,70,30,50,30,30,10,70,60,50,60,30,20,20), 
                   type=c("Can-A","Can-A","Can-A","Can-A","Can-A","Can-B","Can-B","Can-B","Can-B","Can-B","Can-C","Can-C","Can-C","Can-C","Can-C"))
library(dplyr)
group_by(data,type) %>% summarise(count = n(),mean = mean(sale.count, na.rm = TRUE))
# ANOVA
result <- aov(sale.count~type, data = data)
summary(result)



data <- PlantGrowth 
library(dplyr)
group_by(data,group) %>% summarise(count = n(),mean = mean(weight, na.rm = TRUE))
# ANOVA
result <- aov(weight~group, data = data)
summary(result)

# Tukey HSD (Tukey Honest Significant Differences)
TukeyHSD(result)

# Homogeneity of variances (equal variances assumption)
library(car)
leveneTest(weight ~ group, data = data)

# Normality (assumption)
aov_residuals <- residuals(object = result)
# Shapiro-Wilk test
shapiro.test(x = aov_residuals)

# Kruskal-Wallis rank sum test (used when ANOVA assumptions are not met)
kruskal.test(weight~group, data = data)



###################################
#For Credit worthiness dataset
###################################


rm(list=ls())

library(dplyr)
data <- read.csv('CreditWorthiness.csv')
group_by(data,Cpur) %>% summarise(count = n(),mean = mean(Camt, na.rm = TRUE))
# ANOVA
result <- aov(Camt~Cpur, data = data)
summary(result)




# Tukey HSD (Tukey Honest Significant Differences)
TukeyHSD(result)

# Homogeneity of variances (equal variances assumption)
#install.packages('car')
library(car)
leveneTest(Camt~ Cpur, data = data)

# Normality (assumption)
aov_residuals <- residuals(object = result)
# Shapiro-Wilk test
shapiro.test(x = aov_residuals)

# Kruskal-Wallis rank sum test (used when ANOVA assumptions are not met)
kruskal.test(Camt~Cpur, data = data)

