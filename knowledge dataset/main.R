# Title     : main.R
# Author    : wujie
# Time      : 2021/4/2


library(e1071)
library(randomForest)
library(xgboost)
library(MASS)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/knowledge dataset")
source("glasso_knowledge.R")
source("clime_knowledge.R")
source("sgl_knowledge.R")
source("standarize.R")
source("getData.R")


p = 83
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
  if(fun == "sgl") theta = sgl_knowledge(train_data = train_data)$theta
  if(fun == "sample") theta = solve(var(train_data))
  return (theta)
}


best_mse_forecast = function (fun_type){
  mu_1 = apply(early_train_data, 2, mean)
  mu_2 = apply(last_train_data, 2, mean)

  Sigma = solve(get_theta(fun_type, train_data))

  Sigma_11 = Sigma[early_semester, early_semester]
  Sigma_21 = Sigma[last_semester, early_semester]

  predict_values = matrix(0, nrow(last_test_data), ncol(last_test_data))
  for(i in 1:nrow(early_test_data)){
    predict_values[i,] = mu_2 + Sigma_21 %*% solve(Sigma_11) %*% (early_test_data[i,] - mu_1)
  }

  RMSE = sqrt(sum((predict_values - last_test_data) ^ 2) / (nrow(last_test_data) * ncol(last_test_data)))
  MAE = sum(abs(predict_values - last_test_data)) / (nrow(last_test_data) * ncol(last_test_data))
  error = list(RMSE = round(RMSE,3), MAE = round(MAE,3))

  return (error)
}


svr_forcast = function (){
  predict_values = matrix(0, nrow(last_test_data), ncol(last_test_data))

  for(i in 1:ncol(last_test_data)){
    tsm = tune.svm(x = early_train_data,
                   y = last_train_data[,i],
                   gamma = 10^(-4:-1),
                   cost = 10^(1:3)
                  )
    gamma = tsm$best.model$gamma
    cost = tsm$best.model$cost
    svr.model = svm(x = early_train_data,
                    y = last_train_data[,i],
                    type = "eps-regression",
                    kernel = "radial",
                    gamma = gamma,
                    cost = cost
                    )

    predict_values[, i] = predict(svr.model, early_test_data)
  }

  RMSE = sqrt(sum((predict_values - last_test_data) ^ 2) / (nrow(last_test_data) * ncol(last_test_data)))
  MAE = sum(abs(predict_values - last_test_data)) / (nrow(last_test_data) * ncol(last_test_data))
  error = list(RMSE = round(RMSE,3), MAE = round(MAE,3))

  return (error)
}


linear_forecast = function (){
  predict_values = matrix(0, nrow(last_test_data), ncol(last_test_data))

  for(i in 1:ncol(last_test_data)){
    y = last_train_data[, i]
    m = lm(y ~., data = as.data.frame(early_train_data))
    predict_values[, i] = predict(m, as.data.frame(early_test_data))
  }

  RMSE = sqrt(sum((predict_values - last_test_data) ^ 2) / (nrow(last_test_data) * ncol(last_test_data)))
  MAE = sum(abs(predict_values - last_test_data)) / (nrow(last_test_data) * ncol(last_test_data))
  error = list(RMSE = round(RMSE,3), MAE = round(MAE,3))
}


rf_forcast = function (){
  predict_values = matrix(0, nrow(last_test_data), ncol(last_test_data))

  for(i in 1:ncol(last_test_data)){
    m = randomForest::randomForest(early_train_data, last_train_data[, i], mtry = 1, ntree = 300)
    predict_values[, i] = predict(m, early_test_data)
  }

  RMSE = sqrt(sum((predict_values - last_test_data) ^ 2) / (nrow(last_test_data) * ncol(last_test_data)))
  MAE = sum(abs(predict_values - last_test_data)) / (nrow(last_test_data) * ncol(last_test_data))
  error = list(RMSE = round(RMSE,3), MAE = round(MAE,3))

  return (error)
}


# fun_type = c("clime", "glasso", "sample", "sgl")
# errors = matrix(0, 2, 7)
# row_name = c('RMSE', 'MAE')
# col_name = c('clime', 'glasso', 'sample', 'sgl', 'SVR', 'randomForest', 'lm')
# dimnames(errors) = list(row_name, col_name)
#
#
# for(i in 1:4){
#   error = best_mse_forecast(fun_type[i])
#   errors[,i] = c(error$RMSE, error$MAE)
# }
# errors[, 5] = c(svr_forcast()$RMSE, svr_forcast()$MAE)
# errors[, 6] = c(rf_forcast()$RMSE, rf_forcast()$MAE)
# errors[, 7] = c(linear_forecast()$RMSE,linear_forecast()$MAE)
#
#
# print(errors)

get_theta('sgl', train_data)











