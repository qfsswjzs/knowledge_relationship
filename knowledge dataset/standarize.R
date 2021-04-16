# Title     : standarize.R
# Author    : wujie
# Time      : 2021/4/10


standarize = function(x){
  if(is.null(nrow(x))){
    sd = sd(x)
    mean = mean(x)
    x = (x - mean) / sd
    result = list(mean = mean, sd = sd, x = x)
    return (result)
  }else{
    mean = vector("numeric", ncol(x))
    sd = vector("numeric", ncol(x))
    for(i in 1:ncol(x)){
      sd[i] = sd(x[, i])
      mean[i] = mean(x[, i])
      x[, i] = (x[, i] - mean[i]) / sd[i]
    }
    result = list(mean = mean, sd = sd, x = x)
    return (result)
  }
}


inverse_standarize = function(x, y) {
  # x是标准化后返回的list
  if(is.null(nrow(x$x))){
     y = y * x$sd + x$mean
     return (y)
  }else{
     for(i in 1:ncol(y)){
       y[, i] = y[, i] * x$sd[i] + x$mean[i]
     }
     return (y)
  }
}
