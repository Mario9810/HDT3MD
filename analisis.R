#Análisis exploratorio
library(tidyverse)
library("ggpubr")
library("ggplot2")
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering
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

#zona de vivienda 

plot(table(data$MSZoning))
table(data$MSZoning)

#utilidades 

plot(table(data$Utilities))
table(data$Utilities)

#funcionalidades

plot(table(data$Functional))
table(data$Functional)

#contacto con la calle

hist(data$LotFrontage)
hist(dar)


#k-medias
datos <-read.csv("./trin.csv")
na.omit(datos)
irisCompleto<-datos[complete.cases(datos),]
km<-kmeans(datos[,1:7],2,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$SalePrice))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$SalePrice))*100


plotcluster(data2[,1:7],km$cluster) #grafica la ubicación de los clusters

#silueta
silkm<-silhouette(km$cluster,dist(datos[,1:7]))
mean(silkm[,3])

#no. de clusters
wss <- (nrow(datos[,1:7])-1)*sum(apply(datos[,1:4],2,var))
for (i in 2:10) 
  
  wss[i] <- sum(kmeans(datos[,1:7], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")
#visualización
fviz_cluster(km, data = datos[,1:7],geom = "point", ellipse.type = "norm")

#casas más caras por grupo
hist(g1$SalePrice)
hist(g2$SalePrice)

