# Title     : getData.R
# Author    : wujie
# Time      : 2021/4/5


setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")

getData = function (){
  train_data = as.matrix(read.csv("Train.csv", header = T, sep = ','), nrow = 1400)
  test_data = as.matrix(read.csv("Test.csv", header = T, sep = ','), nrow = 600)
  sex_train = vector(mode = "numeric", 1400)
  sex_test = vector(mode = "numeric", 600)

  for(i in 1:2000){
     if(i <= 700) sex_train[i] = "male"
     else if(700 < i && i <= 1400) sex_train[i] = "female"
     else if(1400 < i && i <= 1700) sex_test[i-1400] = "male"
     else sex_test[i-1400] = "female"
  }

  return_value = list(train_data = train_data, test_data = test_data, sex_train = sex_train, sex_test = sex_test)

  return (return_value)
}
