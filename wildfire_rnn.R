# dir.create("~/Downloads/jena_climate", recursive = TRUE)
# download.file(
#   "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
#   "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip"
# )
# unzip(
#   "~/Downloads/jena_climate/jena_climate_2009_2016.csv.zip",
#   exdir = "~/Downloads/jena_climate"
# )
# data_dir <- "~/Downloads/jena_climate"
# fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
# data <- read.csv(fname)

#reference: https://blogs.rstudio.com/tensorflow/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/

#setwd("/Users/hk/Desktop/School/MRHS/11th Grade/R/NN-ML/Wildfire-NN-ML")
setwd("C:\\Users\\kimh2\\Desktop\\Wildfire-NN-ML-master")
data <- read.csv("merra2_calfire_jja_mine.csv") #made up a new csv to make things easier
                                                #could be bad though

#note: running for loop makes loss for "ONE" worse but better for "TWO"
for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,20]>=50){
    data[i,20] <- 1
  } else {
    data[i,20] <- 0
  }
}
data <- data.matrix(data)
train_data <- data[1:250,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 1) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }       

    list(samples, targets)
  }
}

lookback <- 1
step <- 1
delay <- 0
batch_size <- 128

train_gen <- generator( #training
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 450,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator( #validation
  data,
  lookback = lookback,
  delay = delay,
  min_index = 451,
  max_index = 600,
  step = step,
  batch_size = batch_size
)

test_gen <- generator( #testing
  data,
  lookback = lookback,
  delay = delay,
  min_index = 601,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (600 - 451 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 600 - lookback) / batch_size


#ONE======
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, input_shape = list(NULL, dim(data)[[-1]])) %>% sf
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)
#END ONE====

#TWO======
model <- keras_model_sequential() %>% 
  layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.2,
            input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)
