library("ggpubr")
library("ggplot2")
library(dplyr)
library(rpart)
library(caret)
library(tree)
library(rsample) 
library(rpart.plot)
library(randomForest)
library(ipred)
#library(tidyverse)

trainSetGiven = read.csv("./data/train.csv", header = TRUE)
testSetGiven = read.csv("./data/test.csv", header = TRUE)

#Join the two sets of data
dataSet <- bind_rows(trainSetGiven, testSetGiven)
dataSet$Id=NULL
summary(dataSet)
dataSetCompleteNumeric =dataSet[, !sapply(dataSet, is.character)]
dataSetCompleteNumeric = dataSetCompleteNumeric[complete.cases(dataSetCompleteNumeric), ]
dataSet = dataSet[complete.cases(dataSet$SalePrice),]
#caro > 150,000
#barato < 100,000
#medio 100,000 - 150,000
caros = dataSet[dataSet$SalePrice>150000,] 
barato = dataSet[dataSet$SalePrice<100000,] 
medio = dataSet[dataSet$SalePrice>100000 & dataSet$SalePrice<150000,] 

#Proporciones caros
split <- initial_split(caros, prop = .65)
trainCaro <- training(split)
testCaro <- training(split)


#Proporciones medios
split <- initial_split(medio, prop = .65)
trainMedio <- training(split)
testMedio <- training(split)

#Proporciones baratos
split <- initial_split(barato, prop = .65)
trainBarato <- training(split)
testBarato <- training(split)



# Proporcion 35, 65
train <- bind_rows(trainCaro, trainMedio, train)
test  <- bind_rows(testCaro, testMedio, testBarato)

m1 <- rpart(
  formula = SalePrice ~ .,
  data    = train,
  method  = "anova",
  control = list(minsplit = 11, maxdepth = 8, cp = 0.01)
)
rpart.plot(m1, type=4)
m1$variable.importance

plotcp(m1)
save(train,test,m1, file = "Sales.RData")
load("Sales.RData")
pred <- predict(m1, newdata = test)
plot(test$SalePrice, pred)

#Analisis
summary(pred)


RMSE(test$SalePrice, pred)
mean(test$SalePrice - pred)
#Evaluate on the test data set






