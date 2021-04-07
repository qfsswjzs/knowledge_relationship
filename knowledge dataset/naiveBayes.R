# Title     : naiveBayes.R
# Author    : wujie
# Time      : 2021/4/5


library(e1071)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")
source("getData.R")

naive_Bayes = function (){
    data = getData()
    train_data = data$train_data
    test_data = data$test_data
    sex_train = data$sex_train
    sex_test = data$sex_test

    train_data = scale(train_data, center = TRUE, scale = TRUE)
    test_data = scale(test_data, center = TRUE, scale = TRUE)

    m = naiveBayes(train_data, sex_train, laplace = 1)
    m_pre = predict(m, test_data)
    nb.table <- table(actual=sex_test,predict=m_pre)
    nb_ratio <- sum(diag(nb.table))/sum(nb.table)
    print(nb_ratio)
}

naive_Bayes()