#source: https://cran.r-project.org/web/packages/elmNNRcpp/vignettes/extreme_learning_machine.html
#install.packages("elmNNRcpp")
library("elmNNRcpp")

setwd("/Users/hk/Desktop/School/MRHS/11th Grade/R/NN-ML/Wildfire-NN-ML")
data <- read.csv("ml_dly_cal_r3.sel.csv")[,c(1:40,43)]
data <- data[,-c(1,2,3)]

lastVar <- ncol(data)-1
predictVar <- ncol(data)

for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=1.5){
    data[i,predictVar] <- 1
  } else {
    data[i,predictVar] <- 0
  }
}

x_train <- data[1:250, 1:lastVar]
x_test <- data[251:nrow(data), 1:lastVar]

y_train= matrix(data[1:250, predictVar], nrow = length(data[1:250, lastVar]),
             ncol = 1)

fit_elm = elm_train(as.matrix(x_train), y_train, nhid = 1000, actfun = 'purelin',
                    init_weights = "uniform_negative", bias = TRUE, verbose = T)

predict_elm = elm_predict(fit_elm, as.matrix(x_test))

#linear =======================

fit_lm = lm(fpc2~., data = data[1:250, ])
predict_lm = predict(fit_lm, newdata = data[251:nrow(data), ])

rmse = function (y_true, y_pred) {
  out = sqrt(mean((y_true - y_pred)^2))
  out
}

y_test = data[251:nrow(data), predictVar]

#mean square errors ====================

print_results <- function(y_test, predict_elm, predict_lm){
  cat('the rmse error for extreme-learning-machine is :', rmse(y_test, predict_elm[, 1]), '\n')
  cat('the rmse error for linear-model is :', rmse(y_test, predict_lm), '\n')
  cat('range:', min(sqrt((y_test-predict_elm[,1])^2)), 'to', max(sqrt((y_test-predict_elm[,1])^2)))
}

print_results(y_test, predict_elm, predict_lm)

