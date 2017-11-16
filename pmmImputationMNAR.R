
##########################################
## author - Amitsingh Pardeshi           #
##  pmm dataimputation                  #
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
library(class)
path <- ".."
show(path)

#load data
data <- iris

# get summary
summary(data)

dataWolastCol <- data[,-5]
##create missing data 2%,5%,10%, 15%,20%,25%
dataWolastCol_1 <- as.data.frame(data[,1])
dataWolastCol_3 <- as.data.frame(data[,3])
##create missing data 2,5,10, 15,20,25
iris.mis_2_0 <- prodNA(dataWolastCol_1, noNA = 0.04)
iris.mis_2_1 <- prodNA(dataWolastCol_3, noNA = 0.04)

iris.mis_5_0 <- prodNA(dataWolastCol_1, noNA = 0.1)
iris.mis_5_1 <- prodNA(dataWolastCol_3, noNA = 0.1)

iris.mis_10_0 <- prodNA(dataWolastCol_1, noNA = 0.2)
iris.mis_10_1 <- prodNA(dataWolastCol_3, noNA = 0.2)

iris.mis_15_0 <- prodNA(dataWolastCol_1, noNA = 0.3)
iris.mis_15_1 <- prodNA(dataWolastCol_3, noNA = 0.3)

iris.mis_20_0 <- prodNA(dataWolastCol_1, noNA = 0.4)
iris.mis_20_1 <- prodNA(dataWolastCol_3, noNA = 0.4)

iris.mis_25_0 <- prodNA(dataWolastCol_1, noNA = 0.5)
iris.mis_25_1 <- prodNA(dataWolastCol_3, noNA = 0.5)


iris.mis_2 <- cbind(iris.mis_2_0,data[,2],iris.mis_2_1, data[,4])
iris.mis_5 <- cbind(iris.mis_5_0,data[,2],iris.mis_5_1, data[,4])
iris.mis_10 <- cbind(iris.mis_10_0,data[,2],iris.mis_10_1, data[,4])
iris.mis_15 <- cbind(iris.mis_15_0,data[,2],iris.mis_15_1, data[,4])
iris.mis_20 <- cbind(iris.mis_20_0,data[,2],iris.mis_20_1, data[,4])
iris.mis_25 <- cbind(iris.mis_25_0,data[,2],iris.mis_25_1, data[,4])


# check missing values
#summary(iris.mis)

#md.pattern(iris.mis)

#mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
#                 numbers=TRUE, sortVars=TRUE,
#                labels=names(iris.mis), cex.axis=.7,
#               gap=3, ylab=c("Missing data","Pattern"))



## call pmm inputation with iris.mis
iris.pmmImpuated_2 <- mice(iris.mis_2, m=5, maxit = 50, method = 'pmm', seed = 500)
iris.pmmImpuated_5 <- mice(iris.mis_5, m=5, maxit = 50, method = 'pmm', seed = 500)
iris.pmmImpuated_10 <- mice(iris.mis_10, m=5, maxit = 50, method = 'pmm', seed = 500)
iris.pmmImpuated_15<- mice(iris.mis_15, m=5, maxit = 50, method = 'pmm', seed = 500)
iris.pmmImpuated_20<- mice(iris.mis_20, m=5, maxit = 50, method = 'pmm', seed = 500)
iris.pmmImpuated_25 <- mice(iris.mis_25, m=5, maxit = 50, method = 'pmm', seed = 500)

iris.pmmImpuated_2 <- complete(iris.pmmImpuated_2,1) # iris.pmmImpuated_2$data[,1:4]
iris.pmmImpuated_5 <- complete(iris.pmmImpuated_5,1)#iris.pmmImpuated_5$data[,1:4]
iris.pmmImpuated_10 <- complete(iris.pmmImpuated_10,1)#iris.pmmImpuated_10$data[,1:4]
iris.pmmImpuated_15<- complete(iris.pmmImpuated_15,1)#iris.pmmImpuated_15$data[,1:4]
iris.pmmImpuated_20<- complete(iris.pmmImpuated_20,1)#iris.pmmImpuated_20$data[,1:4]
iris.pmmImpuated_25 <- complete(iris.pmmImpuated_25,1)#iris.pmmImpuated_25$data[,1:4]

rmsepmm_2 <- rmse(iris.pmmImpuated_2, dataWolastCol, na.rm = TRUE)
rmsepmm_5 <- rmse(iris.pmmImpuated_5, dataWolastCol, na.rm = TRUE)
rmsepmm_10 <- rmse(iris.pmmImpuated_10, dataWolastCol, na.rm = TRUE)
rmsepmm_15<- rmse(iris.pmmImpuated_15,dataWolastCol ,na.rm = TRUE)
rmsepmm_20 <- rmse(iris.pmmImpuated_20, dataWolastCol, na.rm = TRUE)
rmsepmm_25 <- rmse(iris.pmmImpuated_25, dataWolastCol, na.rm = TRUE)

print(rmsepmm_2)
print(rmsepmm_5)
print(rmsepmm_10)
print(rmsepmm_15)
print(rmsepmm_20)
print(rmsepmm_25)
rmseVecpt <- c(rmsepmm_2, rmsepmm_5, rmsepmm_10, rmsepmm_15, rmsepmm_20, rmsepmm_25)

barplot(rmseVecpt)



##plot RMSE 
barplot(rmsepmm_2, ylab = "RMSE", main="RMSE error distribution for 2%  missing data")
barplot(rmsepmm_5 , ylab = "RMSE", main="RMSE error distribution for 5%  missing data")
barplot(rmsepmm_10, ylab = "RMSE", main="RMSE error distribution for 10%  missing data")
barplot(rmsepmm_15, ylab = "RMSE", main="RMSE error distribution for 15%  missing data")
barplot(rmsepmm_20, ylab = "RMSE", main="RMSE error distribution for 20%  missing data")
barplot(rmsepmm_25, ylab = "RMSE", main="RMSE error distribution for 25%  missing data")



total_RMSE_2 <-  rmsepmm_2[1] + rmsepmm_2[2] + rmsepmm_2[3] + rmsepmm_2[4]
total_RMSE_5 <- rmsepmm_5[1] + rmsepmm_5[2] + rmsepmm_5[3] + rmsepmm_5[4]
total_RMSE_10 <- rmsepmm_10[1] + rmsepmm_10[2] + rmsepmm_10[3] + rmsepmm_10[4]
total_RMSE_15 <- rmsepmm_15[1] + rmsepmm_15[2] + rmsepmm_15[3] + rmsepmm_15[4]
total_RMSE_20 <- rmsepmm_20[1] + rmsepmm_20[2] + rmsepmm_20[3] + rmsepmm_20[4]
total_RMSE_25 <- rmsepmm_25[1] + rmsepmm_25[2] + rmsepmm_25[3] + rmsepmm_25[4]

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
iris_c_2 <-rbind(iris_r, iris.pmmImpuated_2)
normalize <- function(x){
  return( (x -min(x))/ (max(x) - min(x)))
}

iris_n <- as.data.frame(lapply(iris_c_2, normalize))

iris_train <- iris_n[1:150,]
iris_train_test <- iris_n[151:300,]
iris_train_target <- iris[, 5]

iris.pmmImpuated_5_n <- as.data.frame(lapply(iris.pmmImpuated_5, normalize))
iris.pmmImpuated_10_n <- as.data.frame(lapply(iris.pmmImpuated_10, normalize))
iris.pmmImpuated_15_n <- as.data.frame(lapply(iris.pmmImpuated_15, normalize))
iris.pmmImpuated_20_n <- as.data.frame(lapply(iris.pmmImpuated_20, normalize))
iris.pmmImpuated_25_n <- as.data.frame(lapply(iris.pmmImpuated_25, normalize))
iris.pmmImpuated_2_n <- as.data.frame(lapply(iris.pmmImpuated_2, normalize))


iris.pred_2 <- knn(train = iris_train_test, test=iris_train_test, cl = iris_train_target, k =20)
iris.pred_5 <- knn(train = iris.pmmImpuated_5_n, test=iris.pmmImpuated_5_n, cl = iris_train_target, k =20)
iris.pred_10 <- knn(train = iris.pmmImpuated_10_n, test=iris.pmmImpuated_10_n, cl = iris_train_target, k =20)
iris.pred_15 <- knn(train = iris.pmmImpuated_15_n, test=iris.pmmImpuated_15_n, cl = iris_train_target, k =20)
iris.pred_20 <- knn(train = iris.pmmImpuated_20_n, test=iris.pmmImpuated_20_n, cl = iris_train_target, k =20)
iris.pred_25 <- knn(train = iris.pmmImpuated_25_n, test=iris.pmmImpuated_25_n, cl = iris_train_target, k =20)
iris.pred <- knn(train = iris_train, test=iris_train, cl = iris_train_target, k =20)

table(iris_train_target, iris.pred_2)
table(iris_train_target, iris.pred_5)
table(iris_train_target, iris.pred_10)
table(iris_train_target, iris.pred_15)
table(iris_train_target, iris.pred_20)
table(iris_train_target, iris.pred_25)
#table(iris_train_target, iris.pred)




