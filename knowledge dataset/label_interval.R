# Title     : label_interval.R
# Author    : wujie
# Time      : 2021/4/14


label_interval = function (test_label, nbins = 2){
  split_point = (max(test_label) + min(test_label)) / 2

  label = vector("numeric", length(test_label))
  for(i in 1:length(test_label)){
    if(test_label[i] <= split_point) label[i] = 1
    else label[i] = 2
  }

  return.value = list(label = label)
}

test_function = function (){
  t = c(1,2,3,4,5)
  t.label = label_interval(t)
  print(t.label)
}
