# Title     : clime_lambda_selection.R
# Author    : wujie
# Time      : 2021/4/1

library(flare)
library(caret)

source("lossfunction.R")

lambda_selection = function (data, k){
  nlambda = 100
  lambdalist = seq(from = 0.01, to = 1, length.out = nlambda)

  loss = vector(mode = 'numeric', nlambda)
  folds = createFolds(data[,61], k)

  for(i in 1:k){
    testMat = data[folds[[i]],]
    trainMat = data[-folds[[i]],]
    clime_result = sugm(trainMat, method = "clime", lambda = lambdalist, verbose =FALSE)
    wilist = clime_result$icov

    for(j in 1:nlambda){
       wi = matrix(unlist(wilist[j]), nrow = p)
       loss[j] = loss[j] + lossfun(var(testMat), wi)
    }
  }

  index = which(loss == min(loss), arr.ind=TRUE)

  return (lambdalist[index])

}