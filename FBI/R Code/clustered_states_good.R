library(tidyverse)
df <- read.csv("crimes_state_time.csv")
df<-df[,-c(7,17)]
rate<-c(1,2,3,13:21)
total<-c(1:12)
rates_df <- df[,rate]
total_df <- df[,total]
all <- c(3:12)
rates_df$state.year <- paste(rates_df$State,rates_df$Year)

# set row names to the state year
row.names(rates_df)<-rates_df[,13]
# remove the state, year, and state.year columns
rates_df <- rates_df[,-c(1,2,13)]

# normalize input variables
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
rates.df.norm <- sapply(rates_df,rescale)
rates.df.norm

# add row names: utilities
row.names(rates.df.norm)<-row.names(rates_df)

###########################################################
##########################################################
library(tidyverse)  
library(cluster)    
library(factoextra)

fviz_nbclust(rates.df.norm, kmeans, method = "wss")
fviz_nbclust(rates.df.norm, kmeans, method = "silhouette")

k2 <- kmeans(rates.df.norm, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = rates.df.norm)

k3 <- kmeans(rates.df.norm, centers = 3, nstart = 25)
k4 <- kmeans(rates.df.norm, centers = 4, nstart = 25)
k5 <- kmeans(rates.df.norm, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = rates.df.norm) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = rates.df.norm) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = rates.df.norm) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = rates.df.norm) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

fviz_cluster(k4, data=rates.df.norm)
k4$centers

# Cluster 3 ALL DC (lowest pop, highest violent, murder, robbery, aggass, property, larceny, vehicle)
# Cluster 1 - NY, CA, TX (highest pop, high robbery, moderate else)
# Cluser 2 - splits remaining states (lowest levels of most)
# Cluster 4 - splits remaining states (higher pop, higher violent, murder, most rape, 
#                                     aggass, prop, most burgarly, larceny, vehicle)