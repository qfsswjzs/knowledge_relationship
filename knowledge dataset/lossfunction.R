# Title     : lossfunction.R
# Author    : wujie
# Time      : 2021/4/1


lossfun <- function(Sigma, Omega) {
  ot <- as.numeric(unlist(determinant(Omega)))
  if (ot[2]<=0) warning("Precision matrix estimate is not positive definite!")
  tmp <- (sum(diag(Sigma%*%Omega)) - ot[1])
  if(is.finite(tmp)) {
    return(tmp)
  } else {
    return(Inf)
  }
}

