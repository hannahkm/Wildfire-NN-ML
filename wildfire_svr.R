install.packages("e1071")
library("e1071")
library("caret")

setwd("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data")
data <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/ml_dly_cal_r3.sel.csv")[,1:41]
data <- data[c(which(data$month==6),which(data$month==7),which(data$month==8)),]
data <- data[order(data$year),]
k <- 4

num_train <- round(rows*(k-1)/k)
set.seed(314)

predictVar = which(names(data)=="fpc1")

data <- rbind(data[sample(which(data$fpc1==0),length(which(data$fpc1==0))/2),],data[which(data$fpc1!=0),])
rows <- nrow(data)
row_per_fold <- nrow(data)/k

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
  
  prediction <- predict(svm1, test)

  xtab <- table(test$fpc1, prediction)
  df <- as.data.frame(as.matrix(xtab))
  right <- 0
  total <- sum(df[,3])
  for (j in 1:nrow(df)){
    if (df[j,1]==df[j,2]){
      right <- right + df[j,3]
    }
  }
  cat("percent accuracy:", (right/total)*100, "\n")
  cat("percent error:", 100-(right/total)*100, "\n")
}
# percent accuracy: 30.43478 
# percent error: 69.56522 

# percent accuracy: 26.08696 
# percent error: 73.91304 

# percent accuracy: 30.43478 
# percent error: 69.56522 

# percent accuracy: 21.73913 
# percent error: 78.26087 

#from least to greatest accuracy: r3, r2, r1
