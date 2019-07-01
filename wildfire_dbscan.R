# install.packages("fpc")
# install.packages("dbscan")
# install.packages("factoextra")

#so this one isn't going so well lol
#https://stats.stackexchange.com/questions/270631/why-are-most-of-my-points-classified-as-noise-using-dbscan

data <- read.csv("ml_dly_cal_r1.sel.csv")[,1:41]
df <- data[, 4:40]

# Compute DBSCAN using fpc package
# library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 5, MinPts = 3, method="hybrid")
db2 <- dbscan::dbscan(df, 0.9, 3, "raw")

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

