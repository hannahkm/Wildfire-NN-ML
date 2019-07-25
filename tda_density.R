data <- read.csv("/Users/hk/Desktop/School/MRHS/11th\ Grade/R/NN-ML/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")[, c("t2mmax", 
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

df <- as.data.frame(matrix(ncol=ncol(data)))
colnames(df) <- colnames(data)

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
 
vertex.sort <- sort(vertex.size)

for (i in data.mapper2$num_vertices:(data.mapper2$num_vertices-10)) { 
  ind <- which(vertex.size==vertex.sort[[i]])
  points.in.vertex <- data.mapper2$points_in_vertex[[ind]]
  len <- length(points.in.vertex)
  for (j in 1:len) {
    if (!is.na(data[points.in.vertex[[j]], 3])) {
      df<- as.data.frame(rbind(df, data[points.in.vertex[[j]],]))
    }
  }

  df <- df[-1,]
  plot_density2(df, ind)
}

plot_density2 <- function(data_a, ind){
  data_a$frp_aqua <- data_a$frp_aqua+10
  power <- log(data_a$frp_aqua,10)
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
  plot(x=bin_freq_days[,1], y=bin_freq_days[,3], type="p",xlab="bins",ylab="freq",
       main=paste("density of cluster number",ind))
  lines(x=bin_freq_days[,1], y=bin_freq_days[,3], type="l", col = "red")
  #lines(predict(loess(bin_freq_days[,3]~bin_freq_days[,1])))
}


