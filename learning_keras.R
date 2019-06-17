#source: https://keras.rstudio.com/

devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(method = "conda", version="nightly")
library(keras)
use_condaenv('r-tensorflow')
#(need anaconda in order to download and run code)

View(mnist)
#using the classic MNIST handwriting dataset
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape - turns 3d arrays into matrices (flatten images into vectors)
dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)
# rescale - convert grayscale values to 0~1
x_train <- x_train / 255
x_test <- x_test / 255

#one-hot encode vectors into binary class matrices??
#i guess it basically makes it possible to use ML on the input, and then later revert
  #the data type to the original??
  #ref: https://www.quora.com/What-is-one-hot-encoding-and-when-is-it-used-in-data-science
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

#creates a sequential model - stacks of layers
#each pipe (%>%) adds a new layer to the model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax")

summary(model)

#compile the model with loss function, optimizer, and metrics...
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

#train with 30 epochs and batches of 128
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

#loss and accuracy graphs
plot(history)

#see performace on test data
model %>% evaluate(x_test, y_test,verbose = 0)
#percent loss
#percent accuracy 

#predictions on new data?
model %>% predict_classes(x_test)







