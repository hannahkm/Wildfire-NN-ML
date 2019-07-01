devtools::install_github("rstudio/tensorflow")
library(tensorflow)
#install_tensorflow(method = "conda", version="nightly")
install_tensorflow() #<- apparently this works now?!??
library(keras)
use_condaenv('r-tensorflow')

?dataset_cifar10 #to see the help file for details of dataset
cifar<-dataset_cifar10()
