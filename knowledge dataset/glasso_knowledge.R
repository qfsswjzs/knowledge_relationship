# Title     : glasso_knowledge
# Author    : wujie
# Time      : 2021/3/31

library(glasso)

source('glasso_rho_selection.R')

glasso_knowledge = function (data){
  rho = rho_selection(data, k = 7)
  glasso_result = glasso(var(data), rho = rho)
  theta = glasso_result$wi
  return (theta)
}





