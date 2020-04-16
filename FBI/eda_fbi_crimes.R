library(BBmisc)
##################################################
df <- read.csv("fbi_crimes.csv", nrows = 20)
df <- df[,1:20]
summary(df)

sapply(df, class)
cols.num <- c(2,3,5,7,9,11,13,15,17,19,14,18)
df[,cols.num] <- sapply(df[cols.num],as.numeric)
sapply(df, class)

all <- c(2:20)
df[,all] <- sapply(df[all], normalize)
summary(df)
##################################################
library(tidyverse)
ggplot(data=df, aes(Year, Population)) +
  geom_point() + geom_line()

library(ggplot2)
library(reshape2)
d <- melt(df, id.vars="Year")

ggplot(d, aes(Year,value, col=variable)) + 
  geom_point() + geom_line()
#  + stat_smooth() 

# Separate plots
ggplot(d, aes(Year,value, col=variable)) + 
  geom_point(show.legend = FALSE) + 
  stat_smooth(show.legend = FALSE) +
  facet_wrap(~variable)

##################################################
rate <- c(1,2,4,6,8,10,12,14,16,18,20)
rates <- df[,rate]

number <- c(1,2,3,5,7,9,11,13,15,17,19)
numbers <- df[,number]

d_rates <- melt(rates, id.vars="Year")

ggplot(d_rates, aes(Year,value, col=variable)) + 
  geom_point() + geom_line() #+
#  stat_smooth() 

# Separate plots
ggplot(d_rates, aes(Year,value, col=variable)) + 
  geom_point(show.legend = FALSE) + 
  stat_smooth(show.legend = FALSE) +
  facet_wrap(~variable)

d_nums <- melt(numbers, id.vars="Year")

ggplot(d_nums, aes(Year,value, col=variable)) + 
  geom_point() + geom_line()
#  + stat_smooth() 

# Separate plots
ggplot(d_nums, aes(Year,value, col=variable)) + 
  geom_point(show.legend = FALSE) + 
  stat_smooth(show.legend = FALSE) +
  facet_wrap(~variable)

View(df)
