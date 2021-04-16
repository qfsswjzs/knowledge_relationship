# Title     : model.R
# Author    : wujie
# Time      : 2021/3/30

library(MASS)

# data生成的模型

n = 100
vn = 100

get_data = function (model_name, p){
  mean = vector(mode = 'numeric', p)
  theta = matrix(0, nrow = p, ncol = p)
  theta = get_model(model_name = model_name, theta = theta, p)
  sigma = ginv(theta)

  train_data = mvrnorm(n, mean, sigma)
  validation_data = mvrnorm(vn, mean, sigma)
  return_value = list(train_data = train_data, validation_data = validation_data, theta = theta)
  return (return_value)
}


model0 = function (theta, p){
  for(i in 1: p){
    for(j in 1: p){
      if(i == j)  theta[i, j] = 1
      if(j == i + 1) {
         theta[i, j] = 0.5
         theta[j, i] = 0.5
      }
    }
  }
  return (theta)
}

# AR(1) model
model1 = function (theta, p){
  for(i in 1: p){
    for(j in 1: p){
      theta[i, j] = 0.6 ^ abs(i - j)
    }
  }
  return (theta)
}


model2 = function (theta, p){
  p = ncol(theta)
  for(i in 1:p){
    for(j in 1:p){
      if(i < j){
        binom.result = rbinom(1, 1, 0.1)
        if(binom.result == 1)  {
          theta[i, j] = 0.5
          theta[j, i] = 0.5
        }
      }
    }
  }

  eigvals <- Re(eigen(theta, only.values=T)$values)
  perturb <- max(max(eigvals) - p*min(eigvals), 0)/(p-1)
  theta <- theta+diag(p)*perturb
  theta = theta / perturb

  for(i in 1:p){
    for(j in 1:p){
      theta[i, j] = theta[i, j] / sqrt(theta[i, i] * theta[j, j])
    }
  }
  eigvals <- Re(eigen(theta, only.values=T)$values)
 # print(min(eigvals))
  return (theta)
}


model3 = function (theta, p){
  for(i in 1: p){
    for(j in 1: p){
      if(i == j)  theta[i, j] = 1
      else{
         theta[i, j] = 0.5
      }
    }
  }
  return (theta)
}


# Band Grpah
model4 = function(theta, p){
  for(i in 1: p){
    for(j in 1: p){
      if(i == j)  theta[i, j] = 1
      if(j == i + 1) {
         theta[i, j] = 0.6
         theta[j, i] = 0.6
      }
      if(j == i + 2) {
        theta[i, j] = 0.3
        theta[j, i] = 0.3
      }
    }
  }

  D = matrix(0, p, p)
  diag(D) = apply(runif(p, min = 1, max = 5),2, sqrt)
  theta = D %*% theta %*% D

  return (theta)
}


# E-R graph
model5 = function (theta, p){

}


get_model = function (model_name, theta, p){
  if(model_name == "model0") return (model0(theta, p))
  if(model_name == "model1") return (model1(theta, p))
  if(model_name == "model2") return (model2(theta, p))
  if(model_name == "model3") return (model3(theta, p))
  if(model_name == "model4") return (model4(theta, p))
}

