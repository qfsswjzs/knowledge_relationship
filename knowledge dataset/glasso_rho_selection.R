# Title     : glasso_rho_selection.R
# Author    : wujie
# Time      : 2021/4/1


library(glasso)
library(caret)

source("lossfunction.R")

rho_selection = function (data, k){
  nrho = 100
  rholist = seq(from = 0.01, to = 1, length.out = nrho)

  loss = vector(mode = 'numeric', nrho)
  folds = createFolds(data[,61], k)

  for(i in 1:k){
    testMat = data[folds[[i]],]
    trainMat = data[-folds[[i]],]
    glasso_result = glassopath(var(trainMat), rholist = rholist, trace = 0, )
    wiList = glasso_result$wi

    for(j in 1:nrho){
       loss[j] = loss[j] + lossfun(var(testMat), wiList[,,j])
    }
  }

  index = which(loss == min(loss), arr.ind=TRUE)

  return (rholist[index])

}


