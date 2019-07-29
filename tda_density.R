data <- read.csv("/Users/hk/Desktop/School/MRHS/11th\ Grade/R/NN-ML/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")[, c("t2mmax","qv2m", "frp_aqua", "year", "month", "day")]

data.dist <- dist(data[, c(1, 2)])
par(mfrow = c(1, 1))

data.mapper2 <- mapper2D( 
  distance_matrix = dist(data.frame(x = data[, 1], y = data[, 2])),
  filter_values = list(data[,1],data[,2]),
  num_intervals = c(5, 5),
  percent_overlap = 60,
  num_bins_when_clustering = 60
)

data.graph <-
  graph.adjacency(data.mapper2$adjacency, mode = "undirected")

vertex.size <- rep(0, data.mapper2$num_vertices)

for (i in 1:data.mapper2$num_vertices) { #i is the cluster number
  points.in.vertex <- data.mapper2$points_in_vertex[[i]] #pts in cluster
  len <- length(points.in.vertex)
  count <- 0
  for (j in 1:len) {
    if (!is.na(data[points.in.vertex[[j]], 3])) {
      count <- count + abs(data[points.in.vertex[[j]], 3])
    }
  }
  
  vertex.size[i] <- count
  
}

# l <- layout.auto(data.graph)
# plot(data.graph, vertex.label = NA,
#      cex.main=0.5, horizontal=TRUE, vertex.size = vertex.size, layout = l)

vertex.sort <- sort(vertex.size)

data_fp <- read.csv("/Users/hk/Desktop/Fire\ Downloads/all_data.csv")
data_fp$Date <- paste(data_fp$month,data_fp$day,data_fp$year,sep="/")

for (i in data.mapper2$num_vertices:(data.mapper2$num_vertices-10)) { 
  df <- as.data.frame(matrix(ncol=ncol(data_fp)))
  colnames(df) <- colnames(data_fp)
  
  ind <- which(vertex.size==vertex.sort[[i]])
  points.in.vertex <- data.mapper2$points_in_vertex[[ind]]
  len <- length(points.in.vertex)

  for (j in 1:len) {
    row <- points.in.vertex[[j]]
    if (!is.na(data[row, 3])) {
      date <- paste(data[row,"month"],data[row,"day"],data[row,"year"],sep="/")
      df<- as.data.frame(rbind(df, data_fp[which(data_fp$Date==date),]))
    }
  }    

  df <- df[-1,]
  df <- df[which(df$FP_power>=1),]
  
  #print(as.data.frame(table(df$frp_aqua)))
  plot_density2(df, ind, len, 1)
}



plot_density2 <- function(data_a, ind, len, region){
  power <- log(data_a$FP_power,10)
  power_freq <- as.data.frame(table(power))
  power_freq[,1] <- as.numeric(as.character(power_freq[,1]))
  
  num_bins <- ceiling(range(power_freq[,1])[2]/0.05)
  
  bin_split <- bin(power, nbins = num_bins, method = "length")
  bin_freq <- as.data.frame(table(bin_split))
  bin_freq_days <- bin_freq
  bin_names <- data.frame(matrix(ncol = 1))
  
  for (i in 1:nrow(bin_freq_days)){ #because apparently idk how to use sapply
    bin_names <- rbind(bin_names,substr(bin_freq_days[i,1],2,lapply(strsplit(toString(
      bin_freq_days[i,1]), ''), function(x) which(x == ',')-1)))
  }
  bin_names <- bin_names[-1,]
  bin_freq_days <- cbind(bin_names, bin_freq_days)
  
  bin_freq_days[,1] <- as.numeric(as.character(bin_freq_days[,1]))
  bin_freq_days[,3] <- as.numeric(as.character(bin_freq_days[,3]))
  bin_freq_days[,3] <- bin_freq_days[,3]/len
  
  if (region == 1){
    plot(x=bin_freq_days[,1], y=bin_freq_days[,3], type="n",xlab="bins",ylab="freq/day",
         main=paste("density of cluster",ind))
    lines(x=bin_freq_days[,1], y=bin_freq_days[,3], type="l", col = "black")
    #north
    plot_density2(data_a[which(data_a$FP_latitude>=38),], ind, len, 2)
    #south
    plot_density2(data_a[which(data_a$FP_latitude<38),], ind, len, 3)
    
    legend("topright", legend=c("total","north","south"), inset = 0.02,
           col=c("black","red","blue"), lty=1, cex=0.8, box.lty=0)
    
    #split north and south by 38N: general split between biomes
    
  } else if (region == 2){
    lines(x=bin_freq_days[,1], y=bin_freq_days[,3], type="l", col = "red")
  } else {
    lines(x=bin_freq_days[,1], y=bin_freq_days[,3], type="l", col = "blue")
  }
  
  #lines(predict(loess(bin_freq_days[,3]~bin_freq_days[,1])))
}

