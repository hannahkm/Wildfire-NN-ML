#https://www.datacamp.com/community/tutorials/support-vector-machines-r
load(file="C:/Users/kimh2/Downloads/ESL.mixture.rda")
rm(x,y)

attach(ESL.mixture)

plot(x, col = y + 1)

dat = data.frame(y = factor(y), x)
fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)

func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)

#===========================================
#https://medium.com/@ODSC/build-a-multi-class-support-vector-machine-in-r-abcdd4b7dab6

library(e1071)
data(iris)
n <- nrow(iris)
ntrain <- round(n*0.75)
k <- 4

row_per_fold <- round(n/k)
num_train <- round(n*(k-1)/k)

s <- split(iris,(sample(n, k, replace=F)))

prev_index <- 1
for (i in 1:k){
  tindex <- sample(nrow(s[[i]]),round(nrow(s[[i]])*(k-1)/k))
  train_iris <- iris[row.names(s[[i]])[tindex],]
  test_iris <- iris[row.names(s[[i]])[-tindex],]

  svm1 <- svm(Species~., data=train_iris, method = "C-classification", 
              kernal="radial", gamma=0.1, cost=10)
  
  summary(svm1)
  
  plot(svm1, train_iris, Petal.Width~Petal.Length, 
       slice=list(Sepal.Width=3, Sepal.Length=4))
  
  
  prediction <- predict(svm1, test_iris)
  xtab <- table(test_iris$Species, prediction)
  #print(as.matrix(xtab))
  df <- as.data.frame(as.matrix(xtab))
  right <- df[1,3]+df[5,3]+df[9,3]
  total <- nrow(test_iris)
  cat("percent accuracy:", (right/total)*100, "\n")
  cat("percent error:", 100-(right/total)*100, "\n")
}





