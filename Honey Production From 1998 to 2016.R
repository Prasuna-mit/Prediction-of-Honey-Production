#Loading libraries
library(base)
library(dplyr)
library(funModeling)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggbiplot)
library(naniar)
library(magrittr)
library(corrplot)
library(corrgram)
library(Metrics)
library(plyr)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#CSV file extraction and assigning to a new variable
Honey <- read.csv("C:/Users/HoneyProduction1998-2016.csv", head = T)

#View file using differnt methods
View(Honey) 
glimpse(Honey)
summary(Honey)
str(Honey)

#Checking for missing values
gg_miss_var(Honey1)
df_status(Honey1)

#Grouping only numerical variables
honeyNum <- data.frame(Honey)
View(honeyNum)
honeyNum$state <- NULL
#honeyNum$year <- NULL
View(honeyNum)

#Replacing exact rounded prodvalue with exact values
honeyNum$prodvalue <- honeyNum$priceperlb * honeyNum$totalprod

#Checking for the skewness
par(mfrow=c(1,1)) 
par(mfrow=c(3,3))
hist(honeyNum$numcol)
hist(honeyNum$yieldpercol)
hist(honeyNum$totalprod)
hist(honeyNum$stocks)
hist(honeyNum$prodvalue)
#apply logarithm to reduce the skewness
honeyNum$numcol <- log(honeyNum$numcol+1)
hist(honeyNum$numcol)
honeyNum$totalprod <- log(honeyNum$totalprod+1)
hist(honeyNum$totalprod)
honeyNum$stocks <- log(honeyNum$stocks+1)
hist(honeyNum$stocks)
honeyNum$prodvalue <- log(honeyNum$prodvalue+1)
hist(honeyNum$prodvalue)

#Outlier Detection
par(mfrow=c(2,2))
boxplot(honeyNum$stocks, xlab="stocks")
boxplot(honeyNum$numcol, xlab="numcol")
boxplot(honeyNum$yieldpercol, xlab="yieldpercol")
boxplot(honeyNum$prodvalue, xlab="prodvalue")
boxplot(honeyNum$totalprod, xlab="totalprod")
boxplot(Honey)
boxplot(honeyNum)

#correlation plot
#honeyCor <- data.frame(Honey)
#str(honeyCor)
#honeyCor$state <- NULL
#honeyCor$year <- NULL
View(honeyNum)
honeyCor <- data.frame(honeyNum[c(1:7)])
chart.Correlation(honeyCor, histogram=TRUE, pch=19)
corrplot(corrgram(honeyCor))
View(honeyCor)

#Honey test(2013-2016) & train (from 1998-2012)
train1 <- data.frame(honeyCor[c(1:626),])
View(train1)

test1 <- data.frame(honeyCor[-c(1:626),])
View(test1)

#Building linear model of train data
Fit <- lm(data= train1, log(totalprod+1) ~ log(numcol+1)+log(yieldpercol+1)+log(stocks+1)+log(prodvalue+1))
View(Fit)
summary(Fit)
#Predicting the values
Predict <- predict(Fit,train1)
# Calulating Root Mean Square value to check performance evaluation
rmse(log(train1$totalprod+1), Predict)
#Predicting the production for tes
#Performance evaluaition data
Predict1 <- predict(Fit,test1)
rmse(log(test1$totalprod+1), Predict1)
summary(Fit)
plot(Fit)

#Histogram for residuals
ggplot(data=test1, aes(Fit$Residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

#Insights
#1
#year Vs mean production
meanVal <- length(unique(honeyNum$year))
for(i in unique(honeyNum$year)) {
  meanVal[i - unique(honeyNum$year)[1] + 1] <- mean(honeyNum[honeyNum$year == i,]$prodvalue)
}
qplot(x = unique(honeyNum$year), y = meanVal, angle=90, xlab = "Year", ylab = "Mean Production Value in Dollars", main = "Mean Production Value vs Year") +
  geom_smooth(method = "lm")

#2
#Total Production Vs year
meanProd <- length(unique(honeyNum$year))
for(i in unique(honeyNum$year)) {
  meanProd[i - unique(honeyNum$year)[1] + 1] <- mean(honeyNum[honeyNum$year == i,]$totalprod)
}
qplot(x = unique(honeyNum$year), y = meanProd, angle=90, xlab = "Year", ylab = "Total Mean Production in lbs", main = "Mean of Total Production (lbs) vs Year") +
  geom_smooth(method = "lm")

?summary
