# Title     : randomForest.R
# Author    : wujie
# Time      : 2021/4/6


library(randomForest)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")
source("getData.R")

data = getData()
train_data = data$train_data
test_data = data$test_data
sex_train = data$sex_train
sex_test = data$sex_test


random_Forest = function (){
  sex_train = as.factor(sex_train)
  rf = randomForest(train_data, sex_train)
  print(rf)
  pred = predict(rf, test_data)
  rftable = table(sex_test, pred,
                  dnn = c('Actual','Predicted'))
}

random_Forest()
