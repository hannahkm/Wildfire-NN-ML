devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(method = "conda", version="nightly")
library(keras)
use_condaenv('r-tensorflow')

setwd("/Users/hk/Desktop/School/MRHS/11th Grade/R/NN-ML/Wildfire-NN-ML")
data <- read.csv("merra2_active_calfire_jja.csv")

train <- data[,1:17]
test <- data[,20]
for (i in 1:length(test)){
  if (test[[i]]>=50){
    test[[i]] <- 1
  } else {
    test[[i]] <- 0
  }
}
test = to_categorical(test)

# create sequential model
model = keras_model_sequential()

model %>%
  layer_dense(input_shape = ncol(train), units = 10, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax")

# add a loss function and optimizer
model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

# fit model with our training data set, training will be done for 200 times data set
fit = model %>%
  fit(
    x = as.matrix(train),
    y = test,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.3,
    epochs = 200 #epochs = iterations of the algorithm
  )
plot(fit)


#==================================================================================
#reference: https://keras.rstudio.com/

x_train <- data[1:255, c(2, 6:14, 17)]
y_train <- data[1:255, 20]
x_test <- data[256:368, c(2, 6:14, 17)]
y_test <- data[256:368, 20]

#run the for loops in order to make fcount_aqua binary
for (i in 1:length(y_train)){
  if (y_train[[i]]>=50){
    y_train[[i]] <- 1
  } else {
    y_train[[i]] <- 0
  }
}

for (i in 1:length(y_test)){
  if (y_test[[i]]>=50){
    y_test[[i]] <- 1
  } else {
    y_test[[i]] <- 0
  }
}
#one-hot encode vectors into binary class matrices??
#i guess it basically makes it possible to use ML on the input, and then later revert
#the data type to the original??
#x_train <- to_categorical(x_train)
#x_test <- to_categorical(x_test)
y_train <- to_categorical(y_train)
y_test <- to_categorical(y_test)

y_train <- argmax(y_train) #install.packages("ramify")

use_implementation("tensorflow")

model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(ncol(x_train))) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_flatten() %>% 
  layer_dense(units = max(y_train)+1, activation = "softmax")


summary(model)

#compile the model with loss function, optimizer, and metrics...
model %>% compile(
  loss = "sparse_categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

#train with 30 epochs and batches of 128
history <- model %>% fit(
  as.matrix(x_train), as.matrix(y_train), 
  epochs = 500, batch_size = 128, 
  validation_split = 0.2
)
#val_loss: value of cost function for cross-validation data
#loss: value of cost function for training data.


#loss and accuracy graphs
plot(history)

#see performace on test data
#model %>% evaluate(as.matrix(x_test), as.matrix(y_test),verbose = 0)
#percent loss
#percent accuracy 

#predictions on new data?
#model %>% predict_classes(x_test)





