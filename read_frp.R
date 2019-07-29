#install.packages("readr") - for reading multiple files
#install.packages("OneR") - for creating bins from data
library("readr")
library("OneR")

# DON'T RUN AGAIN
# files <- as.data.frame(as.matrix(list.files(
#   path = "E:/data/MYD14", full.names = T)))
# files <- rbind(files, as.data.frame(as.matrix(list.files(
#   path = "E:/data/MOD14", full.names = T))))
# 
# 
# data <- data.frame(matrix(ncol = 5))
# colnames(data) <- c("Date", "Time", "FP_latitude", "FP_longitude", "FP_power")
# 
# for (j in 1:nrow(files)){
#   sub_files <- list.files(path=toString(files[j,]), full.names=T)
#   for (k in 1:length(sub_files)){
#     tbl <- read.table(toString(sub_files[k]),skip=6, header = FALSE,
#                       stringsAsFactors = FALSE,sep=",") 
#     colnames(tbl) <- as.matrix(tbl[1,])
#     tbl <- tbl[-1,c(1,2,6,7,17)]
#     rownames(tbl) <- 1:nrow(tbl)
#     
#     tbl$FP_latitude <- as.numeric(as.character(tbl$FP_latitude))
#     tbl$FP_longitude <- as.numeric(as.character(tbl$FP_longitude))
#     tbl$FP_power <- as.numeric(as.character(tbl$FP_power))
#     
#     # i <- 1
#     # while (i <= nrow(tbl)){
#     #   if (!(tbl$FP_longitude[i]>=-124 && tbl$FP_longitude[i]<=-114 && 
#     #         tbl$FP_latitude[i]>=32.5 && tbl$FP_latitude[i]<=40)){
#     #     tbl <- tbl[-i,]
#     #   } else{
#     #     i <- i+1
#     #   }
#     # }
#     tbl <- tbl[which(tbl$FP_longitude >= -124 & tbl$FP_longitude <= -114 & tbl$FP_latitude >= 32.5 & tbl$FP_latitude <= 40),]
#     
#     data <- rbind(data, tbl)
#   }
#   print(j)
# }
# 
# data <- data[-1,]
# data <- data[-which(data$FP_power==0),]
# write.csv(data, file="all_data.csv", row.names=FALSE)

data <- read.csv("/Users/hk/Desktop/Fire\ Downloads/all_data.csv")[-1,-1]
rownames(data) <- 1:nrow(data)

#======================================= SUMMER ONLY
# df_months <- sapply(data[,1],substring,6,7)
# data <- cbind(df_months, data)

data_summer <- data[which(data$month %in% c("6", "7", "8")),]

power_freq <- as.data.frame(table(log(data_summer$FP_power,10)))
power_freq[,1] <- as.numeric(as.character(power_freq[,1]))

plot(density(power_freq[,1]))

#======================================= ACTIVE SUMMERS

# df_years <- sapply(data$Date,substring,1,4)
# data <- cbind(df_years, data)

data_active <- data[which((data$year %in% 
                    c("2006", "2008", "2015", "2016"))
                    & (data$month %in% c("6", "7", "8"))),]

power_act <- log(data_active$FP_power,10)
power_freq_act <- as.data.frame(table(power_act))
power_freq_act[,1] <- as.numeric(as.character(power_freq_act[,1]))

plot(density(power_freq_act[,1], bw=0.05), col = "red")

#======================================= INACTIVE SUMMERS

data_inactive <- data[which((data$year %in% c("2005", "2007", "2010", "2011"))
                          & (data$month %in% c("6", "7", "8"))),]

power_inact <- log(data_inactive$FP_power,10)
power_freq_inact <- as.data.frame(table(power_inact))
power_freq_inact[,1] <- as.numeric(as.character(power_freq_inact[,1]))

lines(density(power_freq_inact[,1], bw=0.05), col = "blue")


plot_density <- function(data_a, data_i){
  power <- log(data_a$FP_power,10)
  power_freq <- as.data.frame(table(power))
  power_freq[,1] <- as.numeric(as.character(power_freq[,1]))
  
  num_bins <- ceiling(range(power_freq[,1])[2])/0.05
  
  bin_split <- bin(power, nbins = num_bins, method = "length")
  bin_freq <- as.data.frame(table(bin_split))
  bin_freq_days <- bin_freq
  bin_freq_days[,2] <- bin_freq_days[,2]/92
  bin_names <- data.frame(matrix(ncol = 1))
  
  for (i in 1:nrow(bin_freq_days)){ #because apparently idk how to use sapply
    bin_names <- rbind(bin_names,substr(bin_freq_days[i,1],2,lapply(strsplit(toString(
      bin_freq_days[i,1]), ''), function(x) which(x == ',')-1)))
  }
  bin_names <- bin_names[-1,]
  bin_freq_days <- cbind(bin_names, bin_freq_days)
  
  bin_freq_days[,1] <- as.numeric(as.character(bin_freq_days[,1]))
  plot(x=bin_freq_days[,1], y=bin_freq_days[,3], 
       type="n",xlab="bins", ylab="freq/day", 
       main="frequency of fire count \n of active and inactive summers")
  lines(x=bin_freq_days[,1], y=bin_freq_days[,3], type="l", col = "red")
  bin_freq_days[,1] <- as.numeric(as.character(bin_freq_days[,1]))
  #lines(predict(loess(bin_freq_days[,3]~bin_freq_days[,1])))

  power <- log(data_i$FP_power,10)
  power_freq <- as.data.frame(table(power))
  power_freq[,1] <- as.numeric(as.character(power_freq[,1]))
  
  num_bins <- ceiling(range(power_freq[,1])[2])/0.05
  
  bin_split <- bin(power, nbins = num_bins, method = "length")
  bin_freq <- as.data.frame(table(bin_split))
  bin_freq_days <- bin_freq
  bin_freq_days[,2] <- bin_freq_days[,2]/92
  bin_names <- data.frame(matrix(ncol = 1))
  
  for (i in 1:nrow(bin_freq_days)){ #because apparently idk how to use sapply
    bin_names <- rbind(bin_names,substr(bin_freq_days[i,1],2,lapply(strsplit(toString(
      bin_freq_days[i,1]), ''), function(x) which(x == ',')-1)))
  }
  bin_names <- bin_names[-1,]
  bin_freq_days <- cbind(bin_names, bin_freq_days)
  
  bin_freq_days[,1] <- as.numeric(as.character(bin_freq_days[,1]))
  lines(x=bin_freq_days[,1], y=bin_freq_days[,3], type="l", col="blue")
  #lines(predict(loess(bin_freq_days[,3]~bin_freq_days[,1])))
  
  legend("topright", inset = 0.02, legend = c("active", "inactive"),
         col = c("red", "blue"), lty=1, box.lty=0, cex = 0.8)
}

#substr(bin_freq[1,1],2,lapply(strsplit(toString(bin_freq[1,1]), ''), function(x) which(x == ',')-1)) lol
