# Title     : rho_selection.R
# Author    : wujie
# Time      : 2021/3/30

library(glasso)
library(flare)

source("lossfunction.R")


glasso_rho_selection = function (train_data, validation_data){
   rholist = seq(from = 0.01, to = 1.0, length.out = 50)
   glasso_result = glassopath(var(train_data), rholist = rholist, trace = 0)
   wilist = glasso_result$wi
   logliklist = vector(mode = 'numeric', 50)

   for(i in 1:50){
      vs = var(validation_data)
      logliklist[i] = sum(diag(vs %*% wilist[,,i])) - log(det(wilist[,,i]))
   }
   index = which(logliklist == min(logliklist), arr.ind=TRUE)
   rho = rholist[index]

   return (rho)
}


clime_lambda_selection = function (train_data, validation_data){
   p = ncol(train_data)
   lambdalist = seq(from = 1.0, to = 0.01, length.out = 50)
   clime_result = sugm(train_data, method = "clime",lambda = lambdalist, verbose =FALSE)
   wilist = clime_result$icov
   logliklist = vector(mode = 'numeric', 50)

   for(i in 1:50){
      vs = var(validation_data)
      wi = matrix(unlist(wilist[i]), nrow = p)
      logliklist[i] = lossfun(vs, wi)
   }
   index = which(logliklist == min(logliklist), arr.ind=TRUE)
   if(length(index) > 1) index = index[1]
   lambda = lambdalist[index]

   return (lambda)

}




