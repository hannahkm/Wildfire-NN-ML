#source: https://cran.r-project.org/web/packages/elmNNRcpp/vignettes/extreme_learning_machine.html
#install.packages("elmNNRcpp")
library("elmNNRcpp")

data(Boston, package = 'KernelKnn')

Boston = as.matrix(Boston)
dimnames(Boston) = NULL

X = Boston[, -dim(Boston)[2]]
xtr = X[1:350, ] #x_train
xte = X[351:nrow(X), ] #x_test

#convert training data reponse to an one column matrix
ytr = matrix(Boston[1:350, dim(Boston)[2]], nrow = length(Boston[1:350, 
            dim(Boston)[2]]), ncol = 1)

#fit and predict (elmNNRcpp)
fit_elm = elm_train(xtr, ytr, nhid = 1000, actfun = 'purelin', 
                    init_weights = "uniform_negative", bias = TRUE, verbose = T)

pr_te_elm = elm_predict(fit_elm, xte)

#fit and predict (linear)
data(Boston, package = 'KernelKnn')

fit_lm = lm(medv~., data = Boston[1:350, ])

pr_te_lm = predict(fit_lm, newdata = Boston[351:nrow(X), ])

#evaluation metric
rmse = function (y_true, y_pred) {
  out = sqrt(mean((y_true - y_pred)^2))
  out
}

# test data response variable
yte = Boston[351:nrow(X), dim(Boston)[2]]

# mean-squared-error for 'elm' and 'lm'
cat('the rmse error for extreme-learning-machine is :', rmse(yte, pr_te_elm[, 1]), '\n')
cat('the rmse error for linear-model is :', rmse(yte, pr_te_lm), '\n')




