#meant to shift whatever variable needs to be predicted
#data from days ago would be associated with later data
#the flaw in my logic is that the data isn't actually on a daily basis, which would make shifting
#the data by day just a LITTLE inaccurate, so to do: work around the format of the data


stagger <- function(data, columnStagger, days) {
  
}

data <- read.csv("merra2_active_calfire_jja.csv")[, c(2:4, 6:8, 10:17, 20, 22, 23)]
columnStagger <- which(colnames(data) == "fcount_aqua")
days <- 4

name <- names(data)[columnStagger]
stagger_col <- as.data.frame(data[, columnStagger])
data <- data[,-columnStagger]

rows <- nrow(data)
stagger_col <- as.data.frame(stagger_col[-((rows - days + 1):rows),])
df_add <- as.data.frame(c(-1))

names(stagger_col) <- name
names(df_add) <- names(stagger_col)
#identical(names(df_add), names(stagger_col))

for (i in 1:days) {
  stagger_col <- rbind(df_add, stagger_col)
}

currentDay <- currentDay + numDays

data <- cbind(data, stagger_col)


data
