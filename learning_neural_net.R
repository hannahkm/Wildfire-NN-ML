#source: https://tensorflow.rstudio.com/keras/articles/tutorial_basic_regression.html

set.seed(500)
library(MASS)
library(neuralnet)

data <- Boston #some dataset about suburbs in Boston

#checks dataset to make sure nothing is missing
apply(data,2,function(x) sum(is.na(x))) 

View(data) #for more viewing pleasure

#now: randomly split data in training and test set
#fit a linear regression model and test on test set
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test) #mean squared error

#scale and split data - normalize
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

#BEGIN NEURAL NET STUFF (make sure you installed the neuralnet package/library)

n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

#and now... (drumroll...)
plot(nn)
#notes about result:
# black lines: connections between layers and the weight
# blue lines: bias term of each step
#   bias = intercept of some linear model?


#now, to predict values for the test set and calculate mean squared error
#warning: results will be NORMALIZED (because we normalized it before)

pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#compare the results
print(paste(MSE.lm, MSE.nn))
#"21.6297593507225 15.7518370200153"
#so neural net error is less than linear regression (thankfully)

#a more visual comparison of results:
par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

#cross validation to make sure the neural net is actually okay
library(boot)

#this is more the linear regression
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

#now for net
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}
mean(cv.error)

#box plot to see error
par(mfrow=c(1,1))
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
