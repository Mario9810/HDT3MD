#Análisis exploratorio
library(tidyverse)
library("ggpubr")
library("ggplot2")
data<-read.csv("./train.csv")
str(data)

#summary 

summary(data)

#data density
#price
ggdensity(data$SalePrice, 
          main = "precio",
          xlab = "rango")
hist(data$SalePrice)


#area
ggdensity(data$LotArea, 
          main = "Quantity",
          xlab = "Area")
hist(data$LotArea)

#vecindario

plot(table(data$Neighborhood))

#no. de habitaciones
hist(data$BedroomAbvGr)


#no. de baños

hist(data$FullBath)


#no. de cocinas

hist(data$KitchenAbvGr)
