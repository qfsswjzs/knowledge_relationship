

XGBoost = function (){
  sse_error = vector("numeric", ncol(last_test_data))
  sum_mae = vector("numeric", ncol(last_test_data))

  for(i in 1:ncol(last_test_data)){
    xgbr =
    y = last_train_data[, i]
    m = (y ~., data = as.data.frame(early_train_data))
    predict_values = predict(m, as.data.frame(early_test_data))
    sse_error[i] = sum((predict_values - last_test_data[,i]) ^ 2)
    sum_mae[i] = sum(abs(predict_values - last_test_data[,i]))
  }

  RMSE = sum(sse_error) / (nrow(last_test_data) * ncol(last_test_data))
  MAE = sum(sum_mae) / (nrow(last_test_data) * ncol(last_test_data))

  error = list(RMSE = round(RMSE,3), MAE = round(MAE,3))
  return (error)
}