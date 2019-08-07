data <- read.csv("E:/Fire\ Downloads/all_data.csv")

lat_max <- max(data$FP_latitude)
lat_min <- min(data$FP_latitude)
long_max <- max(data$FP_longitude)
long_min <- min(data$FP_longitude)

sqr_len <- 0.1

lat_value <- lat_min
long_value <- long_min

row_num <- round(lat_max-lat_min)/0.1
col_num <- round(long_max-long_min)/0.1

sqr_values <- data.frame(matrix(nrow = row_num, ncol=col_num))
coord_values <- data.frame(matrix(ncol = 3))
colnames(coord_values) <- c("x", "y", "value")
row_ind <- row_num
col_ind <- 1
while(lat_value < lat_max){
  while (long_value < long_max){
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
    add_row <- as.data.frame(cbind(long_value, lat_value, to_add))
    colnames(add_row) <- colnames(coord_values)
    coord_values <- rbind(coord_values, add_row)
    
    long_value <- long_value + sqr_len
    col_ind <- col_ind + 1
  }
  lat_value <- lat_value + sqr_len
  long_value <- long_min
  col_ind <- 1
  row_ind <- row_ind - 1
}

coord_values <- coord_values[-1,]

len <- NROW((coord_values$value))
coord_values2 <- coord_values
coord_values2[which(coord_values2$value<=250),3] <- 0

coord_values2$value <- rev(heat.colors(len))[as.numeric(cut(
  coord_values2$value,breaks=len/0.1))]
par(pty="s")
plot(x=coord_values[,1], y=coord_values[,2], 
     col=coord_values[,3], 
     pch=15, cex = 2.5,
     xlim=c(long_min, long_max), ylim=c(lat_min, lat_max), tck=-.03,
     xlab="longitude", ylab="latitude", main="Fire Power in California",
     family = "serif")

#=================
#install.packages("ggmap")
library("ggmap")
library("maps") 
library("mapdata")
library("ggplot2")
temp <- as.data.frame(coord_values2) 
states <- map_data("state")
ca_nv <- subset(states, region %in% c("california", "nevada"))

coord_values2[which(coord_values2$value=="#FFFFFFFF"),3] <- NA

ggplot(data = ca_nv) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  coord_fixed(1.3) + 
  labs(title="Fire Power", x="Longitude", y="Latitude") +
  theme(
    axis.text = element_text(color="black", size=12, family = "serif"),
    plot.title = element_text(color="black", size=18, hjust=0.5, family = "serif"),
    axis.title.x = element_text(color="black", size=14, family = "serif"),
    axis.title.y = element_text(color="black", size=14, family = "serif")) +
  geom_point(data = coord_values2, 
             mapping = aes(x = x, y = y, color=value), shape=15, size=2) +
  scale_color_identity()

palette(heat.colors(nrow(temp))) 
for (lat in seq(lat_min, lat_max, 0.5)) {
  for (lon in seq(long_min, long_max, 0.5)) { 
    col <- temp[which(temp$x==lat & temp$y==lon), 3] 
    if (!is.na(col)) polygon(lat, lon, col=col, border=NA) 
  } 
}
palette("default") 

