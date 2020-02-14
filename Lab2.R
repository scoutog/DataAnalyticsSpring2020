#Reading in CSV

#Note that I am using "2010EPI_data"
epi <- read.csv("2010EPI_data.csv",skip=1)
View(epi)

#Central Tendancies of EPI variable
summary(epi$EPI)
mean(epi$EPI, na.rm=T)
median(epi$EPI, na.rm=T)

tf = is.na(epi$EPI)
e_epi = epi$EPI[!tf]

mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}
mode(epi$EPI)

#Central Tendancies of DALY variable
summary(epi$DALY)
mean(epi$DALY, na.rm=T)
median(epi$DALY, na.rm=T)
mode(epi$DALY)

#Histograms for EPI and DALY
hist(epi$EPI, breaks=10)
hist(epi$DALY, breaks=10)

#Getting sample_n & sample_frac
library(dplyr)
epi %>% 
  select(EPI) %>% 
  filter(!is.na(EPI)) %>% 
  sample_n(5)

epi %>% 
  select(DALY) %>% 
  filter(!is.na(DALY)) %>% 
  sample_n(5)

epi %>% 
  select(EPI) %>% 
  filter(!is.na(EPI)) %>% 
  sample_frac(1/10)

epi %>% 
  select(DALY) %>% 
  filter(!is.na(DALY)) %>% 
  sample_frac(1/10)

#Using arrange & desc
new_decs_EPI <- epi %>% 
  select(EPI) %>% 
  filter(!is.na(EPI)) %>% 
  arrange(desc(EPI))

new_decs_DALY <- epi %>% 
  select(DALY) %>% 
  filter(!is.na(DALY)) %>% 
  arrange(desc(DALY))

# Mutate to double
epi %>% 
  select(EPI) %>% 
  filter(!is.na(EPI)) %>% 
  mutate(double_EPI = EPI * 2)

epi %>% 
  select(DALY) %>% 
  filter(!is.na(DALY)) %>% 
  mutate(double_DALY = DALY * 2)
#or


# summarize
epi %>%   
  select(EPI) %>% 
  filter(!is.na(EPI)) %>% 
  summarise(mean=mean(EPI))

epi %>%   
  select(DALY) %>% 
  filter(!is.na(DALY)) %>% 
  summarise(mean=mean(DALY))
  
# boxplot
boxplot(epi$ENVHEALTH, epi$ECOSYSTEM)

# qqplot
qqplot(epi$ENVHEALTH, epi$ECOSYSTEM)

#2b regression
south_asia <- epi %>% filter(EPI_regions=="South Asia") %>% filter()

lm(EPI ~ DALY, data=south_asia)

# Import other EPI dataset for linear&least squares
epi_data <- read.csv("EPI_data.csv")

attach(epi_data)
boxplot(ENVHEALTH, DALY, AIR_H, WATER_H)

lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH

summary(lmENVH)

cENVH <- coef(lmENVH)