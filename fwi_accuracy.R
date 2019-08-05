#install.packages("dplyr")
library("dplyr")

#https://www.guru99.com/r-simple-multiple-linear-regression.html#5
#https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R 

setwd("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/")
data <- read.csv("merra2_active_calfire_jja.csv")[,c("fwi", "frp_aqua", "year", "month", "day")]
data$fcount_aqua <- as.numeric(as.character(data$frp_aqua))

model <- frp_aqua~fwi
fit <- lm(model, data)
fit
summary(fit)

anova(fit)
