#source: https://cran.r-project.org/web/packages/elmNNRcpp/vignettes/extreme_learning_machine.html
#install.packages("elmNNRcpp")
library("elmNNRcpp")

setwd("/Users/hk/Desktop/School/MRHS/11th Grade/R/NN-ML/Wildfire-NN-ML")
data <- read.csv("merra2_active_calfire_jja.csv")

x_train <- data[1:250, 1:17]
x_test <- data[251:nrow(data), 1:17]

y_train= matrix(data[1:250, 20], nrow = length(data[1:250, 20]),
             ncol = 1)

fit_elm = elm_train(as.matrix(x_train), y_train, nhid = 1000, actfun = 'purelin',
                    init_weights = "uniform_negative", bias = TRUE, verbose = T)

predict_elm = elm_predict(fit_elm, as.matrix(x_test))

#linear =======================

fit_lm = lm(fcount_aqua~., data = data[1:250, ])
predict_lm = predict(fit_lm, newdata = data[251:nrow(data), ])

rmse = function (y_true, y_pred) {
  out = sqrt(mean((y_true - y_pred)^2))
  out
}

y_test_lm = data[251:nrow(data), 20]

#mean square errors ====================
#note: for some reason, calculating rmse for elm uses the variable y_test_lm 
#      (doesn't work if using y_test)...
cat('the rmse error for extreme-learning-machine is :', rmse(y_test_lm, predict_elm[, 1]), '\n')
#70.92722 
cat('the rmse error for linear-model is :', rmse(y_test_lm, predict_lm), '\n')
#25.55755


