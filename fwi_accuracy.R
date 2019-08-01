tmax_humidity <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/Old Data/merra2_active_calfire_jja.csv")
df <- tmax_humidity[,c("fwi", "fcount_aqua")]
df[,2] <- normalize(df[,2], range=c(min(df$fwi), max(df$fwi)))

num_bins <- ceiling(range(df$fcount_aqua)[2]/3)

bin_split <- bin(df$fcount_aqua, nbins = num_bins, method = "length")
bin_freq <- as.data.frame(table(bin_split))
bin_names <- data.frame(matrix(ncol = 1))

bin_split2 <- bin(df$fwi, nbins = num_bins, method = "length")
bin_freq2 <- as.data.frame(table(bin_split2))
bin_names2 <- data.frame(matrix(ncol = 1))

for (i in 1:NROW(bin_split)){ #because apparently idk how to use sapply
  bin_names <- rbind(bin_names,substr(bin_split[i],2,lapply(strsplit(toString(
    bin_split[i]), ''), function(x) which(x == ',')-1)))

  bin_names2 <- rbind(bin_names2,substr(bin_split2[i],2,lapply(strsplit(toString(
    bin_split2[i]), ''), function(x) which(x == ',')-1)))
}
bin_names <- bin_names[-1,]
bin_names <- as.numeric(as.character(bin_names))
bin_names2 <- bin_names2[-1,]
bin_names2 <- as.numeric(as.character(bin_names2))

sort <- sort(unique(bin_names))
sort2 <- sort(unique(bin_names2))

right <- 0
total <- nrow(df)
for (j in 1:nrow(df)){
  ind1 <- which(sort==bin_names[j])
  ind2 <- which(sort2==bin_names2[j])

  if ((length(ind1)>0 && length(ind2)>0) &&
      (which(sort==bin_names[j]) == which(sort2==bin_names2[j]))){
    right <- right + 1
  }
}
acc <- (right/total)*100
acc

#============================================

num_bins <- ceiling(range(df$fcount_aqua)[2])

bin_split <- bin(df$fcount_aqua, nbins = num_bins, method = "length")
bin_freq <- as.data.frame(table(bin_split))
bin_names <- data.frame(matrix(ncol = 1))
bin_split <- cbind(as.data.frame(matrix(nrow=NROW(bin_split))), 
                   as.data.frame(matrix(nrow=NROW(bin_split))),bin_split)
colnames(bin_split) <- c("min", "max", colnames(bin_split)[-c(1,2)])

for (i in 1:NROW(bin_split)){
  bin_names1 <- substr(bin_split[i,3],2,lapply(strsplit(toString(
    bin_split[i,3]), ''), function(x) which(x == ',')-1))
  
  bin_names2 <- substr(bin_split[i,3],lapply(strsplit(toString(
    bin_split[i,3]), ''), function(x) which(x == ',')+1),nchar(as.character(bin_split[i,3]))-1)
  
  bin_split[i,1] <- bin_names1
  bin_split[i,2] <- bin_names2
}

sort <- sort(unique(bin_names))
sort2 <- sort(unique(bin_names2))

right <- 0
total <- nrow(df)
for (j in 1:nrow(df)){
  ind1 <- which(sort==bin_names[j])
  ind2 <- which(sort2==bin_names2[j])
  
  if ((length(ind1)>0 && length(ind2)>0) &&
      (which(sort==bin_names[j]) == which(sort2==bin_names2[j]))){
    right <- right + 1
  }
}
acc <- (right/total)*100
acc

