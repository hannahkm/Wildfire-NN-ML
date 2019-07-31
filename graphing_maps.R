data <- read.csv("/Users/hk/Desktop/Fire\ Downloads/all_data.csv")

lat_max <- max(data$FP_latitude)
lat_min <- min(data$FP_latitude)
long_max <- max(data$FP_longitude)
long_min <- min(data$FP_longitude)

sqr_len <- 0.5

lat_value <- lat_min
long_value <- long_min

sqr_values <- data.frame(matrix(ncol=1))
while(lat_value < lat_max){
  while (long_value < long_max){
    df <- data[which(data$FP_latitude>=lat_value & 
                       data$FP_latitude<(lat_value+sqr_len) &
                       data$FP_longitude>=long_value & 
                       data$FP_longitude<(long_value+sqr_len)),"FP_power"]
    to_add <- data.frame(c(sum(df)))
  }
}




