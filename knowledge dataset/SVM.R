# Title     : SVM.R
# Author    : wujie
# Time      : 2021/4/5


library(e1071)
library(MASS)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")
source("getData.R")

data = getData()
train_data = data$train_data
test_data = data$test_data
sex_train = data$sex_train
sex_test = data$sex_test

train_data = scale(train_data, center = TRUE, scale = TRUE)
test_data = scale(test_data, center = TRUE, scale = TRUE)

Svm = function (){
  sex_train = as.factor(sex_train)
  sex_test = as.factor(sex_test)

  m = svm(train_data, sex_train, type = "C-classification", kernel = "radial", gamma = 0.01, cost = 10)
  m_pre = predict(m, test_data)
  nb.table <- table(actual=sex_test,predict=m_pre)
  nb_ratio <- sum(diag(nb.table))/sum(nb.table)
  print(nb.table)
  print(nb_ratio)
}

# 选择最优svm参数
svm_test <- function(x,y){
  y = as.factor(y)
  m = tune.svm(x,y, gamma = 10^(-5:-1), cost = 10^(1:3))
  print(m)
  print(m$best.model)
  type <- c('C-classification','nu-classification','one-classification')
  kernel <- c('linear','polynomial','radial','sigmoid')
  pred <- array(0, dim=c(nrow(x),3,4))
  errors <- matrix(0,3,4)
  nb.ratio = matrix(0,3,4)
  dimnames(errors) <- list(type, kernel)
  dimnames(nb.ratio) = list(type, kernel)

  for(i in 1:3){
    for(j in 1:4){
      object = svm(x, y, type = type[i], kernel = kernel[j], cost = 10)
      pred[,i,j] <- predict(object = object, newdata = x)
      nb.table <- table(actual=y,predict=pred[,i,j])
      nb.ratio[i, j] = sum(diag(nb.table))/sum(nb.table)
      if(i > 2) errors[i,j] <- sum(pred[,i,j] != 1)
      else errors[i,j] <- sum(pred[,i,j] != as.integer(y))
    }
  }
  print(nb.ratio)
  return(errors)
}

#svm_test(train_data, sex_train)
Svm()

