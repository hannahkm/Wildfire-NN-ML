install.packages("e1071")
library("e1071")
library("caret")

setwd("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data")

df_results <- data.frame(matrix(nrow=8))

val <- 3
file_name <- paste("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/ml_dly_cal_r",val,".sel.csv", sep="")

data <- read.csv(file_name)[,c(1:3,15:16,41)]
data <- data[c(which(data$month==6),which(data$month==7),which(data$month==8)),]
data <- data[order(data$year),]
data <- data[,-c(1,2,3)]
k <- 4

df_add <- data.frame(matrix(nrow=8))
colnames(df_add) <- paste("r",val,"_fwi_summer", sep="")

set.seed(314)

predictVar = which(names(data)=="fpc1")

num_bins <- ceiling(range(data$fpc1)[2]/15)

bin_split <- bin(data$fpc1, nbins = num_bins, method = "length")
bin_freq <- as.data.frame(table(bin_split))
bin_names <- data.frame(matrix(ncol = 1))

for (i in 1:NROW(bin_split)){ #because apparently idk how to use sapply
  bin_names <- rbind(bin_names,substr(bin_split[i],2,lapply(strsplit(toString(
    bin_split[i]), ''), function(x) which(x == ',')-1)))
}
bin_names <- bin_names[-1,]
bin_names <- as.numeric(as.character(bin_names))

data <- cbind(data, bin_names)

#data <- rbind(data[sample(which(data$fpc1==0),length(which(data$fpc1==0))/2),],data[which(data$fpc1!=0),])
rows <- nrow(data)
row_per_fold <- nrow(data)/k
num_train <- round(rows*(k-1)/k)

s <- split(data,(sample(nrow(data), k, replace=F)))

prev_index <- 1
for (i in 1:k){
  rand_index <- sample(nrow(s[[i]]),round(nrow(s[[i]])*(k-1)/k))
  train <- data[row.names(s[[i]])[rand_index],]
  test <- data[row.names(s[[i]])[-rand_index],]
  
  svm1 <- svm(factor(bin_names) ~ ., data=train,
              method="C-classification", kernel="radial", 
              gamma=0.1, cost=10)
  
  summary(svm1)
  
  prediction <- predict(svm1, test)

  xtab <- table(test$bin_names, prediction)
  df <- as.data.frame(as.matrix(xtab))
  right <- 0
  total <- nrow(test)
  for (j in 1:nrow(df)){
    if (df[j,1]==df[j,2]){
      right <- right + df[j,3]
    }
  }
  # cat("percent accuracy:", (right/total)*100, "\n")
  # cat("percent error:", 100-(right/total)*100, "\n")
  
  acc <- (right/total)*100
  df_add[i,] <- acc
  # cat("====== tuning ====== \n")
  
  svm1_tune <- tune(svm, train.x=train, train$fpc1, kernel="radial")
  
  svm1_post_tune <- svm(factor(bin_names)~., data=train,type='C-classification',
                        kernel = "radial")
  summary(svm1_post_tune)
  
  prediction2 <- predict(svm1_post_tune, test)
  
  xtab <- table(test$bin_names, prediction2)
  #print(as.matrix(xtab))
  df <- as.data.frame(as.matrix(xtab))
  right <- 0
  total <- nrow(test)
  for (j in 1:nrow(df)){
    if (df[j,1]==df[j,2]){
      right <- right + df[j,3]
    }
  }
  acc <- (right/total)*100
  df_add[(i+4),] <- acc
}

df_results <- cbind(df_results, df_add)








