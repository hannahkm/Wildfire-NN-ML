install.packages("e1071")
library("e1071")
library("caret")

setwd("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data")
data <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/ml_dly_cal_r1.sel.csv")[,c(1:3,4:41)]
data <- data[c(which(data$month==6),which(data$month==7),which(data$month==8)),]
data <- data[order(data$year),]
data <- data[,-c(1,2,3)]
k <- 4

set.seed(314)

predictVar = which(names(data)=="fpc1")

num_bins <- ceiling(range(data$fpc1)[2]/10)

bin_split <- bin(unique(data$fpc1), nbins = num_bins, method = "length")
bin_freq <- as.data.frame(table(bin_split))
bin_names <- data.frame(matrix(ncol = 1))

for (i in 1:nrow(bin_freq)){ #because apparently idk how to use sapply
  bin_names <- rbind(bin_names,substr(bin_freq[i,1],2,lapply(strsplit(toString(
    bin_freq[i,1]), ''), function(x) which(x == ',')-1)))
}
bin_names <- bin_names[-1,]
bin_freq <- cbind(bin_names, bin_freq)

data <- rbind(data[sample(which(data$fpc1==0),length(which(data$fpc1==0))/2),],data[which(data$fpc1!=0),])
rows <- nrow(data)
row_per_fold <- nrow(data)/k
num_train <- round(rows*(k-1)/k)

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
  total <- nrow(test)
  for (j in 1:nrow(df)){
    if (df[j,1]==df[j,2]){
      right <- right + df[j,3]
    }
  }
  cat("percent accuracy:", (right/total)*100, "\n")
  cat("percent error:", 100-(right/total)*100, "\n")
  
  cat("====== tuning ====== \n")
  
  svm1_tune <- tune(svm, train.x=train, train$fpc1, kernel="radial")
  
  svm1_post_tune <- svm(factor(fpc1)~., data=train,type='C-classification',
                        kernel = "radial")
  summary(svm1_post_tune)
  
  prediction2 <- predict(svm1_post_tune, test)
  
  xtab <- table(test$fpc1, prediction2)
  #print(as.matrix(xtab))
  df <- as.data.frame(as.matrix(xtab))
  right <- 0
  total <- nrow(test)
  for (j in 1:nrow(df)){
    if (df[j,1]==df[j,2]){
      right <- right + df[j,3]
    }
  }
  cat("percent accuracy:", (right/total)*100, "\n")
  cat("percent error:", 100-(right/total)*100, "\n")
  cat("=============================================== \n")
}
