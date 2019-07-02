
# install.packages("fpc")
# install.packages("dbscan")
# install.packages("factoextra")

#so this one isn't going so well lol
#https://stats.stackexchange.com/questions/270631/why-are-most-of-my-points-classified-as-noise-using-dbscan

data <- read.csv("C:/Users/kimh2/Desktop/Wildfire-NN-ML/ML_Data/ml_dly_cal_r1.sel.csv")[,c(1:3,6,8)]
df <- data[, 4:ncol(data)]

# Compute DBSCAN using fpc package
# library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 2)

# all(db$cluster == db2)

# Plot DBSCAN results
# library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
print(db)
print(db2)

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)
#and apparently the optimal epsilon value is nonexistent :)

