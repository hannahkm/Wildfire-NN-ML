#https://medium.com/@ODSC/build-a-multi-class-support-vector-machine-in-r-abcdd4b7dab6

install.packages("e1071")
library("e1071")

setwd("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old_Data")
data <- read.csv("ml_dly_cal_r3.sel.csv")[,1:41]
data <- data[c(which(data$month==6),which(data$month==7),which(data$month==8)),]
data <- data[order(data$year),]
rows <- nrow(data)
k <- 4
row_per_fold <- nrow(data)/k

num_train <- round(rows*(k-1)/k)
set.seed(314)

predictVar = which(names(data)=="fpc1")

for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=10){
    data[i,predictVar] <- 1
  } else {
    data[i,predictVar] <- 0
  }
}

s <- split(data,(sample(nrow(data), k, replace=F)))

prev_index <- 1
for (i in 1:k){
  rand_index <- sample(nrow(s[[i]]),round(nrow(s[[i]])*(k-1)/k))
  train <- data[row.names(s[[i]])[rand_index],]
  test <- data[row.names(s[[i]])[-rand_index],]
  
  svm1 <- svm(factor(fpc1) ~ ., data=train,
              method="C-classification", kernal="radial", 
              gamma=0.1, cost=10)
  
  summary(svm1)
  
  #plot(svm1, train, isi ~ bui)
  
  prediction <- predict(svm1, test)
  xtab <- table(test$fpc1, prediction)
  print(as.matrix(xtab))
  df <- as.data.frame(as.matrix(xtab))
  wrong <- df[2,3]+df[3,3]
  total <- df[2,3]+df[3,3]+df[1,3]+df[4,3]
  cat("percent error:", (wrong/total)*100, "\n")
  cat("percent accuracy:", 100-(wrong/total)*100, "\n")
}

# percent error: 31.52174 
# percent accuracy: 68.47826 

# percent error: 23.91304 
# percent accuracy: 76.08696 

# percent error: 20.65217 
# percent accuracy: 79.34783 

# percent error: 18.47826 
# percent accuracy: 81.52174 

# from least to greatest accuracy: r2, r1, r3

#=====================================

#y response data frame --> factor
dat <- data.frame(y=factor(y),x)
fit = svm(factor(y) ~ ., data=dat, scale=FALSE, kernel="radial", cost=5)

xgrid = expand.grid(X1=px1, X2=px2)
ygrid = predict(fit, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y+1, pch = 19)

#=====================================

func = predict(fit, xgrid, decision.values=TRUE)
func = attributes(func)$decision

xgrid = expand.grid(X1 = px1, X2 = px2)
ygird = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y+1, pch = 19)

#boundary decided by SVM
contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
#Bayes decision boundary
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)


