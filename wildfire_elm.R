#source: https://cran.r-project.org/web/packages/elmNNRcpp/vignettes/extreme_learning_machine.html
#install.packages("elmNNRcpp")
library("elmNNRcpp")

setwd("/Users/hk/Desktop/School/MRHS/11th Grade/R/NN-ML/Wildfire-NN-ML")
data <- read.csv("merra2_active_calfire_jja.csv")[,c(15,16,20)]

lastVar <- ncol(data)-1
predictVar <- ncol(data)

for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=50){
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

fit_lm = lm(fcount_aqua~., data = data[1:250, ])
predict_lm = predict(fit_lm, newdata = data[251:nrow(data), ])

rmse = function (y_true, y_pred) {
  out = sqrt(mean((y_true - y_pred)^2))
  out
}

y_test = data[251:nrow(data), predictVar]

#mean square errors ====================

cat('the rmse error for extreme-learning-machine is :', rmse(y_test, predict_elm[, 1]), '\n')
cat('the rmse error for linear-model is :', rmse(y_test, predict_lm), '\n')

#RMSE ERRORS FOR...
#straight up prediction of fcount_aqua (values i think 0-100)
#elm: 70.92722 
#lm: 25.55755

#prediction of 0/1 for "more" vs "less" fires (values 0-1)
#elm: 0.4997862 
#lm: 0.3724606

#prediction of 0/1 using specific variables ([,c(2:4,6:8,10:17,20)]) (values 0-1)
#elm: 0.4867378 
#lm: 0.4777222 

#prediction of 0/1 using specific variables ([,c(3,5,8,9,10,11,14,20)])
#elm: 0.446603
#lm: 0.48653

#prediction of 0/1 using specific variables (hot-dry-windy???) ([,c(3,10,11,20)])
#elm: 0.4712681
#lm: 0.4945382




