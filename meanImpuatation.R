
##########################################
## author - Amitsingh Pardeshi           #
##  mean dataimputation                  #
##########################################

## install packages
install.packages("VIM")
install.packages("mice")
install.packages("missForest")
install.packages("imputeR")
install.packages("hydroGOF")

## import libraries
library(mice)
library(missForest)
library(VIM)
library(imputeR)
library(hydroGOF)
path <- ".."
show(path)

#load data
data <- iris

# get summary
summary(data)

dataWolastCol <- data[,-5]
##create missing data 2,5,10, 15,20,25
iris.mis_2 <- prodNA(dataWolastCol, noNA = 0.02)
iris.mis_5 <- prodNA(dataWolastCol, noNA = 0.05)
iris.mis_10 <- prodNA(dataWolastCol, noNA = 0.1)
iris.mis_15 <- prodNA(dataWolastCol, noNA = 0.15)
iris.mis_20 <- prodNA(dataWolastCol, noNA = 0.02)
iris.mis_25 <- prodNA(dataWolastCol, noNA = 0.25)


# check missing values
#summary(iris.mis)

#md.pattern(iris.mis)

#mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
 #                 numbers=TRUE, sortVars=TRUE,
  #                labels=names(iris.mis), cex.axis=.7,
   #               gap=3, ylab=c("Missing data","Pattern"))

## mean data impuation
# takes dataframe as input and replace NA values with mean of the column
meanDataImpuation <- function(df){
  i <- 1
  for(col in names(df)){
  # meanVal <- mean(df[,0], na.rm = TRUE)
   print(i)
   print(df[, i])
   localVector <-  df[,i] 
   localVector[is.na(localVector)]<- mean(localVector[!is.na(localVector)])
   df[,i] <- localVector
    i <- i + 1;
  }
  
  print(df)
  
}
## call meanDataImuation with iris.mis
iris.meanImpuated_2 <- meanDataImpuation(iris.mis_2)
iris.meanImpuated_5 <- meanDataImpuation(iris.mis_5)
iris.meanImpuated_10 <- meanDataImpuation(iris.mis_10)
iris.meanImpuated_15<- meanDataImpuation(iris.mis_15)
iris.meanImpuated_20<- meanDataImpuation(iris.mis_20)
iris.meanImpuated_25 <- meanDataImpuation(iris.mis_25)

rmseMean_2 <- rmse(iris.meanImpuated_2, iris[,-5], na.rm = TRUE)
rmseMean_5 <- rmse(iris.meanImpuated_5, iris[,-5], na.rm = TRUE)
rmseMean_10 <- rmse(iris.meanImpuated_10, iris[,-5], na.rm = TRUE)
rmseMean_15<- rmse(iris.meanImpuated_15,iris[,-5], na.rm = TRUE)
rmseMean_20 <- rmse(iris.meanImpuated_20, iris[,-5], na.rm = TRUE)
rmseMean_25 <- rmse(iris.meanImpuated_25, iris[,-5], na.rm = TRUE)

print(rmseMean_2)
print(rmseMean_5)
print(rmseMean_10)
print(rmseMean_15)
print(rmseMean_20)
print(rmseMean_25)
rmseVecpt <- c(rmseMean_2, rmseMean_5, rmseMean_10, rmseMean_15, rmseMean_20, rmseMean_25)

barplot(rmseVecpt)

  