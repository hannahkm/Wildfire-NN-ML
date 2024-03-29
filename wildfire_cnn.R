devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(method = "conda", version="nightly")
#install_tensorflow() #<- apparently this works now?!??
library(keras)
use_condaenv('r-tensorflow')

data <- read.csv("/home/hannah/Wildfire-NN-ML/ML_Data/ml_dly_cal_r1.sel.csv")[,c(1:3,6,8,10,14,25,28,32,34:41)]
data <- data[,-c(1,2,3)]

for (i in 1:(ncol(data)-1)){
  data[,i] <- normalize(data[,i])
}

predictVar = which(names(data)=="fpc1")


for (i in 1:nrow(data)){ #differentiate between "lots" of fires and less fires
  if (data[i,predictVar]>=5){
    data[i,predictVar] <- 1
  } else {
    data[i,predictVar] <- 0
  }
}

len <- round(nrow(data)*0.75)
train_x <- data[1:len, 1:(ncol(data)-1)]
train_y <- data[1:len, ncol(data)]

test_x <- data[-(1:len), 1:(ncol(data)-1)]
test_y <- data[-(1:len), ncol(data)]

train_x <- array( unlist(train_x), dim=c(dim(train_x),1) )
test_x <- array( unlist(test_x), dim=c(dim(test_x),1) ) 

#a linear stack of layers
model <- keras_model_sequential()
#configuring the Model
model %>%
  #defining a 2-D convolution layer
  layer_conv_1d(
    filter = ncol(train_x),
    kernel_size = 1,
    padding = "same",
    input_shape = dim(train_x)[-1]
  ) %>%
  layer_activation("relu") %>%
  #another 2-D convolution layer
  
  layer_conv_1d(filter = ncol(train_x) , kernel_size = 1)  %>% 
  layer_activation("relu") %>%
  #Defining a Pooling layer which reduces the dimentions of the features map 
  #and reduces the computational 
  #complexity of the model
  layer_max_pooling_1d(pool_size = 2) %>%
  #dropout layer to avoid overfitting
  layer_dropout(0.25) %>%
  layer_conv_1d(filter = ncol(train_x), kernel_size = 1, padding = "same") %>% 
  layer_activation("relu") %>%  
  layer_conv_1d(filter = ncol(train_x), kernel_size = 1) %>%  
  layer_activation("relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_dropout(0.25) %>%
  #flatten the input
  layer_flatten() %>%
  layer_dense(512) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  #output layer-10 classes-10 units
  layer_dense(2) %>%
  #applying softmax nonlinear activation function to the output layer 
  #to calculate cross-entropy
  layer_activation("softmax")
#for computing Probabilities of classes-"logit(log probabilities)


opt<-optimizer_adam( lr= 0.00001 , decay = 1e-6 ) #lr = learning rate (too small makes it really chaotic)
#lr-learning rate , decay - learning rate decay over each update

model %>%
  compile(loss="sparse_categorical_crossentropy",
          optimizer=opt,metrics = "accuracy")
#Summary of the Model and its Architecture
summary(model)

#TRAINING PROCESS OF THE MODEL
  model %>% fit(train_x, as.matrix(train_y) ,batch_size=32,
                 epochs=80,validation_data = list(test_x, as.matrix(test_y)),
                 shuffle=TRUE)
  
  predictions <- predict_classes(model, test_x)
  probs <- predict_proba(model, test_x)
  

resize_data <- function(df){
  new_arr <- array(dim=c(nrow(df),8,6))
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (j<=8){
        new_arr[i,j,] <- df[i,j]
      }
    }
  }
  
}





