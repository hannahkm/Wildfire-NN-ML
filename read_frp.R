install.packages("readr")
library("readr")

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
    
    data <- rbind(data, tbl)
  }
}

write.csv(data, file="all_data.csv")





