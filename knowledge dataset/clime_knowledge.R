# Title     : clime_knowledge.R
# Author    : wujie
# Time      : 2021/4/1


library(flare)


source('clime_lambda_selection.R')
source("glasso_rho_selection.R")

clime_knowledge = function (data){

  lambda = lambda_selection(data, k = 7)
  clime_result = sugm(data, method = "clime", lambda = lambda, verbose = FALSE)
  theta = matrix(unlist(clime_result$icov[1]), nrow = ncol(data))
  return (theta)
}

