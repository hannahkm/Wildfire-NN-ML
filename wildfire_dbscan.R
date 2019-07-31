# install.packages("fpc")
# install.packages("dbscan")
# install.packages("factoextra")

#so this one isn't going so well lol
#https://stats.stackexchange.com/questions/270631/why-are-most-of-my-points-classified-as-noise-using-dbscan

data <- read.csv("E:/Fire\ Downloads/all_data.csv")[,c(1:5)]
df <- data[, 4:ncol(data)]

# Compute DBSCAN using fpc package
# library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 2)

# all(db$cluster == db2)

# Plot DBSCAN results
# library("factoextra")
cplot <- fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
plot(cplot)
print(db)

#============================================

print(db2)

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)
#and apparently the optimal epsilon value is nonexistent :)
