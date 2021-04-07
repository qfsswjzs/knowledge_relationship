# Title     : KLloss.R
# Author    : wujie
# Time      : 2021/4/6


KL_loss = function (C, Sigma){
  KL_loss = -log(det(C)) + sum(diag(C %*% Sigma))
  return (KL_loss)
}