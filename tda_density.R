data <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")[, c("t2mmax", 
                                                                                                             "qv2m", "frp_aqua", "year", "month", "day")] #2,8

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

      df<- as.data.frame(rbind(df, data[points.in.vertex[[j]],]))
    }
  }
  
  # data_active <- df[which((df$years %in% c("2006", "2008", "2015", "2016"))
  #                           & (df$months %in% c("6", "7", "8"))),]
  # data_inactive <- df[which((df$years %in% c("2005", "2007", "2010", "2011"))
  #                             & (df$months %in% c("06", "07", "08"))),]
  
  
  plot_density(data_active)
  
  vertex.size[i] <- count
  
}

plot_density <- function(data_a){
  power <- data_a$frp_aqua
  power_freq <- as.data.frame(table(power))
  power_freq[,1] <- as.numeric(as.character(power_freq[,1]))
  
  num_bins <- ceiling(range(power_freq[,1])[2]/3)
  
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
  
  plot(x=bin_freq_days[,1], y=bin_freq_days[,3], type="n",xlab="bins",ylab="freq/day")
  lines(x=bin_freq_days[,1], y=bin_freq_days[,3], type="l", col = "red")
  bin_freq_days[,1] <- as.numeric(as.character(bin_freq_days[,1]))
  lines(predict(loess(bin_freq_days[,3]~bin_freq_days[,1])))
}


