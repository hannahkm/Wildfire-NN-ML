#source: https://cran.r-project.org/web/packages/elmNNRcpp/vignettes/extreme_learning_machine.html
#install.packages("elmNNRcpp")
library("elmNNRcpp")

setwd("/Users/hk/Desktop/School/MRHS/11th Grade/R/NN-ML/Wildfire-NN-ML")
data <- read.csv("/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/ml_dly_cal_r2.sel.csv")[,c(1:41)]
data <- data[,-c(1,2,3)]

lastVar <- ncol(data)-1
predictVar <- ncol(data)

for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=15){
    data[i,predictVar] <- 1
  } else {
    data[i,predictVar] <- 0
  }
}

# data <- rbind(data[sample(which(data$fpc1==0),length(which(data$fpc1==1))),],
#               data[which(data$fpc1==1),])

x_train <- data[1:(nrow(data)*0.75), 1:lastVar]
x_test <- data[(nrow(data)*0.75+1):nrow(data), 1:lastVar]

y_train= matrix(data[1:(nrow(data)*0.75), predictVar], nrow = length(data[1:(nrow(data)*0.75), lastVar]),
             ncol = 1)

fit_elm = elm_train(as.matrix(x_train), y_train, nhid = 1000, actfun = 'purelin',
                    init_weights = "uniform_negative", bias = TRUE, verbose = T)

y_test = as.data.frame(data[(nrow(data)*0.75+1):nrow(data), predictVar])

plot_values <- data.frame(matrix(ncol=2))
colnames(plot_values) <- c("V1", "V2")
#PR
for (i in 0:17){
  predict_elm = elm_predict(fit_elm, as.matrix(x_test))
  roc.plot(as.matrix(y_test),predict_elm)
  threshold <- 0.1+i*0.05
  predict_elm <- ifelse(predict_elm<threshold,0,1)
  # xtab <- table(y_test, predict_elm)
  # #print(as.matrix(xtab))
  # df <- as.data.frame(as.matrix(xtab))
  # plot_values <- rbind(plot_values,calc_pr(df))
  pred <- prediction(predict_elm,y_test)
  perf <- performance(pred,"prec","rec") 
  PRcurve(predict_elm,y_test)
  colnames(predict_elm) <- c("predict")
  colnames(y_test) <- c("class")
  
}

plot_values <- plot_values[-1,]

plot_values <- plot_values[complete.cases(plot_values),]
colnames(plot_values) <- c("Recall", "Precision")

if (nrow(plot_values > 0)){
  plot(x=plot_values[,1], y=plot_values[,2],xlab="Recall",
       ylab="Precision",type="b")
} else{
  cat("you messed up!")
}

#=========================================================

plot_values <- data.frame(matrix(ncol=2))
colnames(plot_values) <- c("V1", "V2")
#ROC
for (i in 0:17){
  predict_elm = elm_predict(fit_elm, as.matrix(x_test))
  threshold <- 0.1+i*0.05
  predict_elm <- ifelse(predict_elm<threshold,0,1)
  xtab <- table(y_test, predict_elm)
  #print(as.matrix(xtab))
  df <- as.data.frame(as.matrix(xtab))
  plot_values <- rbind(plot_values,calc_roc(df))
}
plot_values <- plot_values[-1,]
plot_values <- plot_values[complete.cases(plot_values),]
colnames(plot_values) <- c("False_Pos", "True_Pos")

if (nrow(plot_values > 0)){
  plot(x=plot_values[,1], y=plot_values[,2],xlab="False Pos",
       ylab="True Pos",type="b")
} else{
  cat("you messed up!")
}


calc_pr <- function(df){
  #precision = true pos/(true pos + false pos)
  #recall = true pos/(true pos + false neg)
  if (any(df[,1]==1 && df[,2]==1)){
    true_pos <- df[which(df[,1]==1&&df[,2]==1),3]
  } else{
    true_pos <- 0
  }
  
  if (any(df[,1]==0 && df[,2]==1)){
    false_pos <- df[which(df[,1]==0&&df[,2]==1),3]
  } else{
    false_pos <- 0
  }
  
  if (any(df[,1]==1 && df[,2]==0)){
    false_neg <- df[which(df[,1]==1&&df[,2]==0),3]
  } else{
    false_neg <- 0
  }
  precision <- df[4,3]/(df[4,3]+df[3,3])
  recall <- df[4,3]/(df[4,3]+df[2,3])
  as.data.frame(matrix(c(recall,precision),ncol=2))
}

calc_roc <- function(df){
  if (any(df[,1]==1 && df[,2]==1)){
    true_pos <- df[which(df[,1]==1&&df[,2]==1),3]
  } else{
    true_pos <- 0
  }
  
  if (any(df[,1]==0 && df[,2]==1)){
    false_pos <- df[which(df[,1]==0&&df[,2]==1),3]
  } else{
    false_pos <- 0
  }
  
  if (any(df[,1]==1 && df[,2]==0)){
    false_neg <- df[which(df[,1]==1&&df[,2]==0),3]
  } else{
    false_neg <- 0
  }
  
  if (any(df[,1]==0 && df[,2]==0)){
    true_neg <- df[which(df[,1]==0&&df[,2]==0),3]
  } else{
    true_neg <- 0
  }
  #false pos (x)= false pos/(false pos + true neg)
  #true pos (y)= true pos/(true pos + false neg)
  spec <- false_pos/(false_pos+true_neg)
  sens <- true_pos/(true_pos + false_neg)
  as.data.frame(matrix(c(spec,sens),ncol=2))
}

#linear =======================

fit_lm = lm(fpc2~., data = data[1:250, ])
predict_lm = predict(fit_lm, newdata = data[251:nrow(data), ])

rmse = function (y_true, y_pred) {
  out = sqrt(mean((y_true - y_pred)^2))
  out
}

#mean square errors ====================

print_results <- function(y_test, predict_elm, predict_lm){
  cat('the rmse error for extreme-learning-machine is :', rmse(y_test, predict_elm[, 1]), '\n')
  cat('the rmse error for linear-model is :', rmse(y_test, predict_lm), '\n')
  cat('range:', min(sqrt((y_test-predict_elm[,1])^2)), 'to', max(sqrt((y_test-predict_elm[,1])^2)))
}



print_results(y_test, predict_elm, predict_lm)

