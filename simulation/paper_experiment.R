# Title     : glasso_experiment
# Author    : wujie
# Time      : 2021/3/30


library(glasso)
library(flare)
library(MASS)
library(utils)

setwd("/Users/wujie/Documents/Projects/Pycharm/graduation_project/simulation")
source('model.R')
source('rho_selection.R')


method_experiment = function (name, train_data, validation_data){
  if(name == "glasso"){
    rho = glasso_rho_selection(train_data = train_data, validation_data = validation_data)
    glasso_result = glasso(var(train_data), rho = rho)
    return (glasso_result$wi)
  }
  if(name == "clime"){
    p = ncol(train_data)
    lambda = clime_lambda_selection(train_data = train_data, validation_data = validation_data)
    clime_result = sugm(train_data, method = "clime", lambda = lambda, verbose = FALSE)
    wi = matrix(unlist(clime_result$icov[1]), nrow = p)

    return (wi)
  }
}


repeat_experiment = function (p, model_name, fun_name){
  data = get_data(model_name = model_name, p)
  theta = data$theta
  train_data = data$train_data
  validation_data = data$validation_data

  wi = method_experiment(name = fun_name, train_data = train_data, validation_data = validation_data)
  error_operator = norm(wi - theta, "2")
  error_l1 = norm(wi - theta, "1")
  error_f = sqrt(sum((wi - theta)^2))
  return_value = list(error_operator = error_operator, error_l1 = error_l1, error_f = error_f)

  return (return_value)
}

plist = c(30, 60, 90, 120, 200)
#plist = c(60, 90, 120, 200)
#plist = c(120)
model_list = c("model1", "model2", "model3")
#model_list = c("model1", "model3")
function_list = c("glasso", "clime")
#function_list = c("clime")
#model_list = c("model2")
#function_name = "clime"

for(function_name in function_list){
   for(model_name in model_list){
     for(p in plist){
        cat("p =", p, model_name, function_name, "\n")
        tpb = txtProgressBar(style = 3)
        error_operator_list = vector(mode = 'numeric', 100)
        error_l1_list = vector(mode = 'numeric', 100)
        error_f_list = vector(mode = 'numeric', 100)

        #i = 1
        for(i in 1:1){
          experiment_result = repeat_experiment(p, model_name, function_name)
          error_operator_list[i] = experiment_result$error_operator
          error_l1_list[i] = experiment_result$error_l1
          error_f_list[i] = experiment_result$error_f
          setTxtProgressBar(tpb, i/100)
        }

        close(tpb)
d
        operator_mean = mean(error_operator_list)
        l1_mean = mean(error_l1_list)
        f_mean = mean(error_f_list)
        operator_se = sd(error_operator_list) / 10
        l1_se = sd(error_l1_list) / 10
        f_se = sd(error_f_list) / 10

        cat('\n', operator_mean, operator_se, '\n')
        cat(l1_mean, l1_se, '\n')
        cat(f_mean, f_se, '\n')

     }
   }
}












