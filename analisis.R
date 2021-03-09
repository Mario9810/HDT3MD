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
trainModel <- bind_rows(trainCaro, trainMedio, trainBarato)
test  <- bind_rows(testCaro, testMedio, testBarato)

m1 <- rpart(
  formula = SalePrice ~ .,
  data    = trainModel,
  method  = "anova"
)
#Plot the tree
rpart.plot(m1, type=4)
#Check importance variables
head(m1$variable.importance, 6)

plotcp(m1)

pred <- predict(m1, newdata = test)
plot(test$SalePrice, pred)

#Análisis
summary(pred)

#SacamosRMSE
RMSE(test$SalePrice, pred)

#La mediana entre la diferencia de ambos data sets
abs(mean(test$SalePrice - pred))
test.SalePriceOrdered <- test[order(test$SalePrice),]$SalePrice
pred.ordered <- pred[order(pred)]
porciento = 25/100
str(test.SalePriceOrdered)

#Get the quarters for the test dataset
fQTest<-test.SalePriceOrdered[1:(942*.25)]
sQTest<-test.SalePriceOrdered[(942*.25):(942*.50)]
tQTest<-test.SalePriceOrdered[(942*.50):(942*.75)]
fQTest<-test.SalePriceOrdered[(942*.75):942]

#Get the quarters for the pred dataset
fQP<-pred.ordered[1:(942*.25)]
sQP<-pred.ordered[(942*.25):(942*.50)]
tQP<-pred.ordered[(942*.50):(942*.75)]
fQP<-pred.ordered[(942*.75):942]

print("Mean error 1 Q")
abs(mean(fQTest - fQP))

print("Mean error 2 Q")
abs(mean(sQTest - sQP))

print("Mean error 3 Q")
abs(mean(tQTest - tQP))

print("Mean error final Q")
abs(mean(fQTest - fQP))

print("Los errores de prediccion se hayaron el el primer y segundo quarto principalmente donde se encuentra los mas caros y mas baratos hay mucha desviacion entre los valores")


#Random FOREST
# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
set.seed(1234)
# Run the model
summary(trainModel)
trainCopy <- trainModel
trainCopy$LotFrontage = NULL
trainCopy$MasVnrArea = NULL
trainCopy$GarageYrBlt = NULL
trainCopy$Alley = NULL
trainCopy$BsmtQual = NULL
trainCopy$MasVnrType = NULL
trainCopy$BsmtCond = NULL
trainCopy$BsmtExposure = NULL
trainCopy$BsmtFinType1 = NULL
trainCopy$BsmtFinType2 = NULL
trainCopy$GarageType = NULL
trainCopy$FireplaceQu = NULL
trainCopy$GarageQual = NULL
trainCopy$GarageCond = NULL
trainCopy$Fence = NULL
trainCopy$PoolQC = NULL
trainCopy$MiscFeature = NULL
trainCopy$GarageFinish = NULL
sapply(trainCopy, function(x)any(is.na(x)))

summary(trainCopy)

modeloRF1<-randomForest(SalePrice~.,data=trainCopy)

#SacamosRMSE
RMSE(test$SalePrice, prediccion)

prediccion<-predict(modeloRF1,newdata = trainCopy)
prediccion.ord = prediccion[order(prediccion)]
#Get the mean of the difference bewteen both
abs(mean(test$SalePrice-prediccion))

str(test.SalePriceOrdered)

#Get the quarters for the test dataset
fQTest<-test.SalePriceOrdered[1:(942*.25)]
sQTest<-test.SalePriceOrdered[(942*.25):(942*.50)]
tQTest<-test.SalePriceOrdered[(942*.50):(942*.75)]
fQTest<-test.SalePriceOrdered[(942*.75):942]

#Get the quarters for the pred dataset
fQP<-prediccion.ord[1:(942*.25)]
sQP<-prediccion.ord[(942*.25):(942*.50)]
tQP<-prediccion.ord[(942*.50):(942*.75)]
fQP<-prediccion.ord[(942*.75):942]


print("Mean error 1 Q")
abs(mean(fQTest - fQP))

print("Mean error 2 Q")
abs(mean(sQTest - sQP))

print("Mean error 3 Q")
abs(mean(tQTest - tQP))

print("Mean error final Q")
abs(mean(fQTest - fQP))

#Plot the result of the Model
varImpPlot(modeloRF1)
plot(test$SalePrice, prediccion)






