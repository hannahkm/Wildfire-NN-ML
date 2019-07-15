#install.packages("readr") - for reading multiple files
#install.packages("OneR) - for creating bins from data
library("readr")
library("OneR")

files <- as.data.frame(as.matrix(list.files(
  path = "/Users/hk/Desktop/Fire\ Downloads/006/MYD14", full.names = T)))
files <- rbind(files, as.data.frame(as.matrix(list.files(
  path = "/Users/hk/Desktop/Fire\ Downloads/006/MOD14", full.names = T))))


data <- data.frame(matrix(ncol = 5))
colnames(data) <- c("Date", "Time", "FP_latitude", "FP_longitude", "FP_power")

for (j in 1:nrow(files)){
  sub_files <- list.files(path=toString(files[j,]), full.names=T)
  for (k in 1:length(sub_files)){
    tbl <- read.table(toString(sub_files[k]),skip=6, header = FALSE,
                      stringsAsFactors = FALSE,sep=",") 
    colnames(tbl) <- as.matrix(tbl[1,])
    tbl <- tbl[-1,c(1,2,6,7,17)]
    rownames(tbl) <- 1:nrow(tbl)
    
    tbl$FP_latitude <- as.numeric(as.character(tbl$FP_latitude))
    tbl$FP_longitude <- as.numeric(as.character(tbl$FP_longitude))
    tbl$FP_power <- as.numeric(as.character(tbl$FP_power))
    
    i <- 1
    while (i <= nrow(tbl)){
      if (!(tbl$FP_longitude[i]>=-124 && tbl$FP_longitude[i]<=-114 && 
            tbl$FP_latitude[i]>=32.5 && tbl$FP_latitude[i]<=40)){
        tbl <- tbl[-i,]
      } else{
        i <- i+1
      }
    }
    # data[which(tbl$FP_longitude >= -124 & tbl$FP_longitude <= -114
    #            & tbl$FP_latitude >= 32.5 & tbl$FP_latitude <= 40),]
    
    data <- rbind(data, tbl)
  }
  print(j)
}

data <- data[-1,]
data <- data[-which(data$FP_power==0),]
write.csv(data, file="all_data.csv", row.names=FALSE)

#======================================= SUMMER ONLY

df_months <- sapply(data[,1],substring,6,7)
data <- cbind(df_months, data)

data_summer <- data[which(data$df_months %in% c("06", "07", "08")),]

power_freq <- as.data.frame(table(log(data_summer$FP_power,10)))
power_freq[,1] <- as.numeric(as.character(power_freq[,1]))

plot(density(power_freq[,1]))

#======================================= ACTIVE SUMMERS

df_years <- sapply(data$Date,substring,1,4)
data <- cbind(df_years, data)

data_active <- data[which((data$df_years %in% c("2006", "2008", "2015", "2016"))
                          & (data$df_months %in% c("06", "07", "08"))),]

power_freq_act <- as.data.frame(table(log(data_active$FP_power,10)))
power_freq_act[,1] <- as.numeric(as.character(power_freq_act[,1]))

plot(hist(power_freq_act[,1]), col = "red")

#======================================= INACTIVE SUMMERS

data_inactive <- data[which((data$df_years %in% c("2005", "2007", "2010", "2011"))
                          & (data$df_months %in% c("06", "07", "08"))),]

power_freq_inact <- as.data.frame(table(log(data_inactive$FP_power,10)))
power_freq_inact[,1] <- as.numeric(as.character(power_freq_inact[,1]))

lines(hist(power_freq_inact[,1]), col = "blue")





num_bins <- ceiling(range(power_freq[,1])[2])/0.05

bin_split <- bin(power_freq[,1], nbins = num_bins, method = "length")
bin_freq <- as.data.frame(table(bin_split))


#substr(strsplit(toString(bin_split[1]),","),2,length(toString(bin_split[1])))

