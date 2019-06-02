devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(method = "conda", version="nightly")
library(keras)
use_condaenv('r-tensorflow')

setwd("/Users/hk/Desktop/School/MRHS/11th Grade/R/NN-ML/Wildfire-NN-ML")
data <- read.csv("merra2_active_calfire_jja.csv")

train <- data[,1:17]
test <- data[,20]
test = to_categorical(as.integer(test), num_classes = max(test)+1)

# create sequential model
model = keras_model_sequential()

model %>%
  layer_dense(input_shape = ncol(train), units = 10, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

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
    y = as.matrix(test),
    shuffle = T,
    batch_size = 5,
    validation_split = 0.3,
    epochs = 200
  )
