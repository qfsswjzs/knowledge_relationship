# Title     : classifaction.R
# Author    : wujie
# Time      : 2021/4/14


library(e1071)
library(randomForest)
library(xgboost)
library(MASS)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")
source("glasso_knowledge.R")
source("clime_knowledge.R")
source("label_interval.R")
source("standarize.R")
source("getData.R")


p = 83
k = 10
data = getData()
train_data = data$train_data
test_data = data$test_data

early_semester = 1:59
last_semester = 60:83

early_train_data = train_data[, early_semester]
early_test_data = test_data[, early_semester]
last_train_data = train_data[, last_semester]
last_test_data = test_data[, last_semester]


get_theta = function (fun, train_data){
  if(fun == "clime") theta = clime_knowledge(train_data)
  if(fun == "glasso") theta = glasso_knowledge(train_data)
  if(fun == "sample") theta = solve(var(train_data))
  return (theta)
}


precision_lda_train = function (train_data, label, fun_type){
  mu = matrix(0, 2, ncol(train_data))
  pi = vector("numeric", 2)
  train_data.sd = apply(train_data, 2, sd)

  for(j in 1:ncol(train_data)){
    train_data[, j] = train_data[, j] / train_data.sd[j]
  }

  freq = table(label)
  for(i in 1:2){
    pi[i] = freq[[i]] / length(label)
  }

  for(i in 1:nrow(train_data)){
    mu[label[i], ] = mu[label[i],] + train_data[i,] / (freq[[label[i]]])
  }

  theta = get_theta(fun_type, train_data)
  return.value = list(theta = theta, mu = mu, pi = pi, sd = train_data.sd)

  return (return.value)
}

precision_lda_predict = function(model, test_data){
  sd = model$sd
  theta = model$theta
  mu = model$mu
  pi = model$pi
  lda = vector("numeric", 2)
  label = vector("numeric", nrow(test_data))

  for(j in 1:ncol(test_data)){
    test_data[, j] = test_data[, j] / sd[j]
  }

  for(i in 1:nrow(test_data)){
    for(j in 1:2){
      x = test_data[i,]
      lda[j] = x %*% theta %*% mu[j,] - 0.5 * mu[j, ] %*% theta %*% mu[j,] + log(pi[j])
    }
    label[i] = which(lda == max(lda), arr.ind=TRUE)
  }

  return.value = list(label = label)
  return (return.value)
}

precison.mainfunction = function (fun_type){
  nb.ratio = vector("numeric", ncol(last_train_data))

  for(i in 1:ncol(last_train_data)){
    train.label = label_interval(last_train_data[, i])$label
    test.real.label = label_interval(last_test_data[, i])$label
    m = precision_lda_train(early_train_data, train.label, fun_type)
    test.pred.label = precision_lda_predict(m, early_test_data)$label
    nb.table <- table(actual = test.real.label, predict = test.pred.label)
    nb.ratio[i] = sum(diag(nb.table))/sum(nb.table)
  }

  acc = mean(nb.ratio)
  return.value = list(acc = acc)

  return (return.value)
}

print(precison.mainfunction("clime"))