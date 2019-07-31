#https://medium.com/@ODSC/build-a-multi-class-support-vector-machine-in-r-abcdd4b7dab6

# install.packages("e1071")
library("e1071")

k <- 4
df_results <- data.frame(matrix(nrow=2*k))

setwd("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old_Data")
data <- read.csv("/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/ml_dly_cal_r3.sel.csv")[,c(1:3,15:16,41)]
data <- data[c(which(data$month==6),which(data$month==7),which(data$month==8)),]
data <- data[order(data$year),]
data <- data[,-c(1,2,3)]
df_add <- data.frame(matrix(nrow=8))
colnames(df_add) <- c("r3_fwi_summer")

set.seed(314)

predictVar = which(names(data)=="fpc1")

for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=10){
    data[i,predictVar] <- 1
  } else {
    data[i,predictVar] <- 0
  }
}

data <- rbind(data[sample(which(data$fpc1==0),length(which(data$fpc1==1))),],
              data[which(data$fpc1==1),])
rows <- nrow(data)
row_per_fold <- round(nrow(data)/k)
num_train <- round(rows*(k-1)/k)

s <- split(data,(sample(nrow(data), k, replace=F)))

prev_index <- 1
for (i in 1:k){
  rand_index <- sample(nrow(s[[i]]),round(nrow(s[[i]])*(k-1)/k))
  train <- data[row.names(s[[i]])[rand_index],]
  test <- data[row.names(s[[i]])[-rand_index],]
  
  svm1 <- svm(factor(fpc1) ~ ., data=train,
              method="C-classification", kernel="radial", 
              gamma=0.1, cost=10)
  
  summary(svm1)
  
  #plot(svm1, train, isi ~ bui)
  
  prediction <- predict(svm1, test)
  xtab <- table(test$fpc1, prediction)
  #print(as.matrix(xtab))
  df <- as.data.frame(as.matrix(xtab))
  wrong <- df[2,3]+df[3,3]
  total <- nrow(test)
  # cat("percent error:", (wrong/total)*100, "\n")
  # cat("percent accuracy:", 100-(wrong/total)*100, "\n")
  # 
  # cat("====== tuning ====== \n")
  
  acc <- 100-(wrong/total)*100
  df_add[i,] <- acc
  
  svm1_tune <- tune(svm, train.x=train, train$fpc1, kernel="radial")
  
  svm1_post_tune <- svm(factor(fpc1)~., data=train,type='C-classification',
                        kernel = "radial")
  summary(svm1_post_tune)
  
  prediction2 <- predict(svm1_post_tune, test)
  
  xtab <- table(test$fpc1, prediction2)
  #print(as.matrix(xtab))
  df <- as.data.frame(as.matrix(xtab))
  wrong <- df[2,3]+df[3,3]
  total <- nrow(test)
  # cat("percent error:", (wrong/total)*100, "\n")
  # cat("percent accuracy:", 100-(wrong/total)*100, "\n")
  # cat("=============================================== \n")
  acc <- 100-(wrong/total)*100
  df_add[(i+4),] <- acc
}
df_results <- cbind(df_results, df_add)

#=====================================
data <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/ml_dly_cal_r1.sel.csv")[,c(1:3, 6,8,10,14,25,28,32,34:41)]
# data <- data[c(which(data$month==6),which(data$month==7),which(data$month==8)),]
data <- data[order(data$year),]
data <- data[,-c(1,2,3)]

predictVar = which(names(data)=="fpc1")

for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=10){
    data[i,predictVar] <- 1
  } else {
    data[i,predictVar] <- 0
  }
}

# y response data frame --> factor
x<-data[,(1:ncol(data)-1)]
y<-data[,ncol(data)]

dat <- data.frame(y=factor(y),x)
fit = svm(factor(y) ~ ., data=dat, scale=FALSE, kernel="radial", cost=5)

rand_index <- sample(nrow(x),round(nrow(x)*3/4))
train <- x[row.names(x)[rand_index],]
test <- x[row.names(x)[-rand_index],]

ygrid = predict(fit, train)

plot(train, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y+1, pch = 19)

func = predict(fit, train, decision.values = TRUE)
func = attributes(func)$decision

contour(train, test, matrix(func, nrow(train), nrow(test)), level = 0, add = TRUE)
contour(train, test, matrix(func, nrow(train), nrow(test)), level = 0.5, add = TRUE, col = "blue", lwd = 2)

#=====================================

# func = predict(fit, xgrid, decision.values=TRUE)
# func = attributes(func)$decision
# 
# xgrid = expand.grid(X1 = px1, X2 = px2)
# ygird = predict(fit, xgrid)
# plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
# points(x, col = y+1, pch = 19)
# 
# #boundary decided by SVM
# contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
# #Bayes decision boundary
# contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)