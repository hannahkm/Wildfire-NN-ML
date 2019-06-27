#install.packages("e1071")
library("e1071")

load(file = "ESL.mixture.rda")
names(ESL.mixture)

attach(ESL.mixture)

plot(x,col=y+1)

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


