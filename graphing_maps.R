data <- read.csv("/Users/hk/Desktop/Fire\ Downloads/all_data.csv")

lat_max <- max(data$FP_latitude)
lat_min <- min(data$FP_latitude)
long_max <- max(data$FP_longitude)
long_min <- min(data$FP_longitude)

sqr_len <- 0.5

lat_value <- lat_min
long_value <- long_min

row_num <- round(lat_max-lat_min)/0.5
col_num <- round(long_max-long_min)/0.5

sqr_values <- data.frame(matrix(nrow = row_num, ncol=col_num))
row_ind <- row_num
col_ind <- 1
while(lat_value < lat_max){
  while (long_value < long_max){
    print(lat_value)
    df <- data[which(data$FP_latitude>=lat_value & 
                       data$FP_latitude<(lat_value+sqr_len) &
                       data$FP_longitude>=long_value & 
                       data$FP_longitude<(long_value+sqr_len)),"FP_power"]
    if (NROW(data)==0){
      to_add <- 0
    } else{
      to_add <- sum(df)
    }
    sqr_values[row_ind, col_ind] <- to_add
    
    long_value <- long_value + sqr_len
    col_ind <- col_ind + 1
  }
  lat_value <- lat_value + sqr_len
  long_value <- long_min
  col_ind <- 1
  row_ind <- row_ind - 1
}




