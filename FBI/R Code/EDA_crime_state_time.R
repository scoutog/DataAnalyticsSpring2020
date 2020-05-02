library(tidyverse)
df <- read.csv("crimes_state_time.csv")
df<-df[,-c(7,17)]
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
df[,c(3:21)] <- sapply(df[,c(3:21)], rescale)
rate<-c(1,2,3,13:21)
total<-c(1:12)
rates_df <- df[,rate]
total_df <- df[,total]

all <- c(3:12)

library(BBmisc)

fips <- read.csv("us-state-ansi-fips.csv")
names(fips)[names(fips) == "stname"] <- "State"

rates_df <- left_join(rates_df, fips, by = "State")
total_df <- left_join(total_df, fips, by = "State")

rates2010 <- subset(rates_df, Year == 2010)
names(rates2010)[names(rates2010) == "State"] <- "state"
names(rates_df)[names(rates_df) == "State"] <- "state"
names(total_df)[names(total_df) == "State"] <- "state"
##########
library(gtrendsR)
library(usmap)

orange <- "#0C95BC"

plot_usmap(data = rates2010, values = "Population",  color = orange, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Popularity", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Population in 2010", caption = "Source: FBI UCR")


plot_usmap(data = rates2010, values = "Population", include =  c(.south_atlantic, .mid_atlantic, .new_england ), color = orange, labels=TRUE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Popularity", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "US East Coast Population", caption = "Source: FBI UCR")


#################
summary(rates_df$Year)

x <- "2014violent.jpg"
y <- 2014
z <- "Violent Crime Total in 2014"

#jpeg(x, width = 568, height = 376)
plot_usmap(data = subset(total_df, Year == y), values = "Violent.crime.total",  color = orange, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Violent Crime", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = z, caption = "Source: FBI UCR")
#dev.off() 

############


library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(choroplethrMaps)
library(choroplethr)
library(tm)
library(wordcloud)
library(RColorBrewer)

by_state <- rates_df  %>% filter(Year == 2014) %>% group_by(state) %>% select(state, Year, Violent.Crime.rate) %>% 
  arrange(desc(Violent.Crime.rate))

head(by_state, 10)
tail(by_state, 10)

ggplot(head(by_state, 10), aes(reorder(state, -Violent.Crime.rate), Violent.Crime.rate, fill = Violent.Crime.rate)) + 
  geom_bar(stat = "identity") + xlab("State") + ylab("Violent Crime Rate in 2014") + 
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4, face = "bold"), 
        plot.title = element_text(size = 20, face = "bold", vjust = 2), 
        axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35), 
        axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + 
  theme(legend.position = "none" )

ggplot(tail(by_state, 10), aes(reorder(state, -Violent.Crime.rate), Violent.Crime.rate, fill = Violent.Crime.rate)) + 
  geom_bar(stat = "identity") + xlab("State") + ylab("Violent Crime Rate in 2014") + 
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4, face = "bold"), 
        plot.title = element_text(size = 20, face = "bold", vjust = 2), 
        axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35), 
        axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + 
  theme(legend.position = "none" )


top.states <- head(rates_df  %>% filter(Year == 1997) %>% group_by(state) %>% select(state, Year, Violent.Crime.rate) %>% 
       arrange(desc(Violent.Crime.rate)),10)




i <- 1998
while (i < 2015) {
  top.states <- rbind(top.states, head(rates_df  %>% filter(Year == i) %>% group_by(state) %>% select(state, Year, Violent.Crime.rate) %>% 
                                         arrange(desc(Violent.Crime.rate)),10))
  i = i+1
}

counts <- table(top.states$state)
counts <- as.data.frame(counts)
counts <- counts[counts$Freq != 0,]

ggplot(counts, aes(Var1, Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + xlab("State") + ylab("# Times Appeared in Top 10 Violent Crime Rate") + 
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4, face = "bold"), 
        plot.title = element_text(size = 20, face = "bold", vjust = 2), 
        axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35), 
        axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + 
  theme(legend.position = "none" )

top.states[top.states$state == "Maryland",]
top.states[top.states$state == "North Carolina",]
top.states[top.states$state == "Ohio",]


###################

by_state <- total_df  %>% filter(Year == 2014) %>% group_by(state) %>% select(state, Year, Legacy.rape..1) %>% 
  arrange(desc(Legacy.rape..1))

head(by_state, 10)
tail(by_state, 10)

ggplot(head(by_state, 10), aes(reorder(state, -Legacy.rape..1), Legacy.rape..1, fill = Legacy.rape..1)) + 
  geom_bar(stat = "identity") + xlab("State") + ylab("Rape Crime Total in 2014") + 
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4, face = "bold"), 
        plot.title = element_text(size = 20, face = "bold", vjust = 2), 
        axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35), 
        axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + 
  theme(legend.position = "none" )

ggplot(tail(by_state, 10), aes(reorder(state, -Legacy.rape..1), Legacy.rape..1, fill = Legacy.rape..1)) + 
  geom_bar(stat = "identity") + xlab("State") + ylab("Rape Crime Total in 2014") + 
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4, face = "bold"), 
        plot.title = element_text(size = 20, face = "bold", vjust = 2), 
        axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35), 
        axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + 
  theme(legend.position = "none" )


top.states <- head(total_df  %>% filter(Year == 1997) %>% group_by(state) %>% select(state, Year, Legacy.rape..1) %>% 
                     arrange(desc(Legacy.rape..1)),10)




i <- 1998
while (i < 2015) {
  top.states <- rbind(top.states, head(total_df  %>% filter(Year == i) %>% group_by(state) %>% select(state, Year, Legacy.rape..1) %>% 
                                         arrange(desc(Legacy.rape..1)),10))
  i = i+1
}

counts <- table(top.states$state)
counts <- as.data.frame(counts)
counts <- counts[counts$Freq != 0,]

ggplot(counts, aes(Var1, Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + xlab("State") + ylab("Rape Crime Total in 2014") + 
  theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4, face = "bold"), 
        plot.title = element_text(size = 20, face = "bold", vjust = 2), 
        axis.title.x = element_text(face = "bold", size = 15, vjust = -0.35), 
        axis.title.y = element_text(face = "bold", vjust = 0.35, size = 15)) + 
  theme(legend.position = "none" )


##################
rates20 <- subset(rates_df, Year == 1997)


plot_usmap(data = rates20, values = "Violent.Crime.rate", include =  c(.north_central_region), color = orange, labels=TRUE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Violent Crimes Rate", label = scales::comma, 
                         limits = c(80,870),) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "North Central US Violent Crime Rate", caption = "Source: FBI UCR")



ggplot(data=rates_df, aes(Year, Violent.Crime.rate)) +
  geom_point() +
  geom_smooth(color='red')
