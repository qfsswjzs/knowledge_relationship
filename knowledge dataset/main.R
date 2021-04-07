# Title     : main.R
# Author    : wujie
# Time      : 2021/4/2


library(MASS)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")
source("glasso_knowledge.R")
source("clime_knowledge.R")
source("LDAfunction.R")
source("KLloss.R")
source("getData.R")


p = 83
data = getData()
train_data = data$train_data
test_data = data$test_data
sex_test = data$sex_test


get_theta = function (fun){
  if(fun == "clime") theta = clime_knowledge(train_data)
  if(fun == "glasso") theta = glasso_knowledge(train_data)
  return (theta)
}


precision_classification = function (){
  for(i in 1:nrow(train_data)){
    for(j in 1:ncol(train_data)){
      train_data[i, j] = train_data[i, j] / sd(train_data[i,])
    }
  }

  for(i in 1:nrow(test_data)){
    for(j in 1:ncol(test_data)){
      test_data[i, j] = test_data[i, j] / (sd(test_data[i,]))
    }
  }

  mu_male = vector(mode = "numeric", p)
  mu_female = vector(mode = "numeric", p)
  male_pi = 0.5
  female_pi =0.5

  for(i in 1:1400){
    if(i <= 700) mu_male = mu_male + train_data[i,]
    else mu_female = mu_female + train_data[i,]
  }

  mu_male = mu_male / 700
  mu_female = mu_female / 700

  mu_list = list(mu_male = mu_male, mu_female = mu_female)
  pi_list = list(male_pi = male_pi, female_pi = female_pi)

  sex = vector(mode = "character", p)
  for(i in 1:600){
    sex[i] = LDAfunction(theta, mu_list, pi_list, test_data[i,])
  }

  TP = 0
  FP = 0
  TN = 0
  FN = 0

  for(i in 1:600){
    if(i <= 300){
      if(sex[i] == "male") TP = TP + 1
      else FN = FN + 1
    }else{
      if(sex[i] == "female") TN = TN  + 1
      else FP = FP + 1
    }
  }
  ACC = (TP + TN) / (TP + TN + TN + FP)
  specifity = TN / (TN + FP)
  sensitivity = TP / (TP + FN)
  MCC = (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  cat(ACC, specifity, sensitivity, MCC, "\n")
}

fun_type = c("clime", "glasso")
for(i in 1:2){
  theta = get_theta(fun_type[i])
}



