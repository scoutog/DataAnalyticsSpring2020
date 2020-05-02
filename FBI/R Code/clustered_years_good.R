library(BBmisc)
df <- read.csv("fbi_crimes.csv", nrows = 20)
df <- df[,1:20]
colnames(df)[colnames(df) == 'ï..Year'] <- 'Year'
summary(df)

sapply(df, class)
cols.num <- c(2,3,5,7,9,11,13,15,17,19,14,18)
df[,cols.num] <- sapply(df[cols.num],as.numeric)
sapply(df, class)

all <- c(2:20)
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100

df[,all] <- sapply(df[all], rescale)
summary(df)

rate <- c(1,2,4,6,8,10,12,14,16,18,20)
rates_df <- df[,rate]

number <- c(1,2,3,5,7,9,11,13,15,17,19)
numbers_df <- df[,number]

row.names(rates_df)<-rates_df[,1]
rates_df <- rates_df[,-1]

colnames(rates_df) <- c("Pop","Violent","Murder","Rape","Robbery","Assault","Prop","Burg","Larceny","Vehicle")

library(tidyverse)  
library(cluster)    
library(factoextra)

fviz_nbclust(rates_df, kmeans, method = "wss")
fviz_nbclust(rates_df, kmeans, method = "silhouette")

k2 <- kmeans(rates_df, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = rates_df)

k3 <- kmeans(rates_df, centers = 3, nstart = 25)
k4 <- kmeans(rates_df, centers = 4, nstart = 25)
k5 <- kmeans(rates_df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = rates_df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = rates_df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = rates_df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = rates_df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

fviz_cluster(k3, data=rates_df)
k3$centers


fviz_cluster(k5, data=rates_df)
k5$centers

#2016 highest pop, lowest prop, burg, larceny

#1997 lowest pop, highest every other crime

set.seed(123)

