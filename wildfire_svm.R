#install.packages("e1071")
library("e1071")

data <- read.csv("merra2_active_calfire_jja.csv")[,c(2:4,6:8,10:17,20)]
rows <- nrow(data)
num_train <- round(rows*0.75)
set.seed(314)

predictVar = which(names(data)=="fcount_aqua")

for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=50){
    data[i,predictVar] <- 1
  } else {
    data[i,predictVar] <- 0
  }
}

rand_index <- sample(rows, num_train)
train <- data[rand_index,]
test <- data[-rand_index,]

svm1 <- svm(data$fcount_aqua, data=train, 
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)

summary(svm1)

plot(svm1, train, data$isi ~ data$bui, slice = list(isi = 4, bui = 2))




#=====================================

#y response data frame --> factor
dat <- data.frame(y=factor(y),x)
fit = svm(factor(y) ~ ., data=dat, scale=FALSE, kernel="radial", cost=5)

xgrid = expand.grid(X1=px1, X2=px2)
ygrid = predict(fit, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y+1, pch = 19)

#=========================================================

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


