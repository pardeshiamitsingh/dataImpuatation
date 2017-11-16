
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
install.packages("gmodels")
install.packages("class")

## import libraries
library(mice)
library(missForest)
library(VIM)
library(imputeR)
library(hydroGOF)
library(gmodels)
library(class)

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

## calculate RMSE
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

##plot RMSE 
barplot(rmseMean_2, ylab = "RMSE", main="RMSE error distribution for 2%  missing data")
barplot(rmseMean_5 , ylab = "RMSE", main="RMSE error distribution for 5%  missing data")
barplot(rmseMean_10, ylab = "RMSE", main="RMSE error distribution for 10%  missing data")
barplot(rmseMean_15, ylab = "RMSE", main="RMSE error distribution for 15%  missing data")
barplot(rmseMean_20, ylab = "RMSE", main="RMSE error distribution for 20%  missing data")
barplot(rmseMean_25, ylab = "RMSE", main="RMSE error distribution for 25%  missing data")

total_RMSE_2 <-  rmseMean_2[1] + rmseMean_2[2] + rmseMean_2[3] + rmseMean_2[4]
total_RMSE_5 <- rmseMean_5[1] + rmseMean_5[2] + rmseMean_5[3] + rmseMean_5[4]
total_RMSE_10 <- rmseMean_10[1] + rmseMean_10[2] + rmseMean_10[3] + rmseMean_10[4]
total_RMSE_15 <- rmseMean_15[1] + rmseMean_15[2] + rmseMean_15[3] + rmseMean_15[4]
total_RMSE_20 <- rmseMean_20[1] + rmseMean_20[2] + rmseMean_20[3] + rmseMean_20[4]
total_RMSE_25 <- rmseMean_25[1] + rmseMean_25[2] + rmseMean_25[3] + rmseMean_25[4]

total_rmse <- c(total_RMSE_2,total_RMSE_5, total_RMSE_10, total_RMSE_15, total_RMSE_20,total_RMSE_25)

per_col <- c(2,5,10,15,20,25)

rmse_df <- data.frame(percentage = per_col,error = total_rmse);
barplot(rmse_df$error,  ylab = "RMSE", main="RMSE error distribution from 2% to 25%  missing data")

## calculate classification error
lebal <- iris[,5]


#set.seed(9850)

#gp<- runif(nrow(iris))

#iris_r <- iris[order(gp)]
iris_r <- iris_r[,c(1,2,3,4)]
iris_c_2 <-rbind(iris_r, iris.meanImpuated_2)
normalize <- function(x){
  return( (x -min(x))/ (max(x) - min(x)))
}

iris_n <- as.data.frame(lapply(iris_c_2, normalize))

iris_train <- iris_n[1:150,]
iris_train_test <- iris_n[151:300,]
iris_train_target <- iris[, 5]

iris.meanImpuated_5_n <- as.data.frame(lapply(iris.meanImpuated_5, normalize))
iris.meanImpuated_10_n <- as.data.frame(lapply(iris.meanImpuated_10, normalize))
iris.meanImpuated_15_n <- as.data.frame(lapply(iris.meanImpuated_15, normalize))
iris.meanImpuated_20_n <- as.data.frame(lapply(iris.meanImpuated_20, normalize))
iris.meanImpuated_25_n <- as.data.frame(lapply(iris.meanImpuated_25, normalize))
iris.meanImpuated_2_n <- as.data.frame(lapply(iris.meanImpuated_2, normalize))


iris.pred_2 <- knn(train = iris_train_test, test=iris_train_test, cl = iris_train_target, k =20)
iris.pred_5 <- knn(train = iris.meanImpuated_5_n, test=iris.meanImpuated_5_n, cl = iris_train_target, k =20)
iris.pred_10 <- knn(train = iris.meanImpuated_10_n, test=iris.meanImpuated_10_n, cl = iris_train_target, k =20)
iris.pred_15 <- knn(train = iris.meanImpuated_15_n, test=iris.meanImpuated_15_n, cl = iris_train_target, k =20)
iris.pred_20 <- knn(train = iris.meanImpuated_20_n, test=iris.meanImpuated_20_n, cl = iris_train_target, k =20)
iris.pred_25 <- knn(train = iris.meanImpuated_25_n, test=iris.meanImpuated_25_n, cl = iris_train_target, k =20)
iris.pred <- knn(train = iris_train, test=iris_train, cl = iris_train_target, k =20)

 table(iris_train_target, iris.pred_2)
 table(iris_train_target, iris.pred_5)
 table(iris_train_target, iris.pred_10)
 table(iris_train_target, iris.pred_15)
 table(iris_train_target, iris.pred_20)
 table(iris_train_target, iris.pred_25)
 table(iris_train_target, iris.pred)

