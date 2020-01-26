# Scout Oatman-Gaitan
# Lab 1
# January 23, 2020

days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c(T,T,F,F,T,T,F)
help("data.frame")
rpi_weather_week <- data.frame(days,temp,snowed)

rpi_weather_week
head(rpi_weather_week)
str(rpi_weather_week)
summary(rpi_weather_week)

rpi_weather_week[1,]
rpi_weather_week[,1]

rpi_weather_week[,'snowed']
rpi_weather_week[,'days']
rpi_weather_week[,'temp']
rpi_weather_week[1:5,c('days','temp')]
rpi_weather_week$temp
subset(rpi_weather_week,subset=snowed==T)

sorted.snowed <- order(rpi_weather_week['snowed'])
sorted.snowed
rpi_weather_week[sorted.snowed,]

dec.snow <- order(-rpi_weather_week$temp)
dec.snow

empty.dataframe <- data.frame() 
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1,col.name.2 = v2)
df

write.csv(df, file = 'saved_df1.csv')
df2 <- read.csv("saved_df1.csv")
df2

library(readxl)
gpw <- read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')
head(gpw)

epi.xls <- read_xls('2010EPI_data.xls')
head(epi.xls)

# this dataframe doesn't seem to provide useful information

summary(gpw$PopulationPerUnit)
summary(gpw$Area)
plot(gpw$Area,gpw$PopulationPerUnit)
boxplot(gpw$PopulationPerUnit, gpw$Area)

data()
help(data)

# EXCERCISE 1

EPI_data <- read.csv('2010EPI_data.csv', skip = 1)
View(EPI_data)

attach(EPI_data)
fix(EPI_data)
EPI

tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI)
fivenum(EPI, na.rm=T)
stem(EPI)
hist(EPI)
hist(EPI, seq(30.,95.,1.0), prob=T)
lines(density(EPI,na.rm=T,bw="SJ"))
rug(EPI)

plot(ecdf(EPI),do.points=F,verticals=T)
par(pty="s")
qqnorm(EPI)
qqline(EPI)

x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)

plot(ecdf(DALY),do.points=F,verticals=T)
par(pty="s")
qqnorm(DALY)
qqline(DALY)

plot(ecdf(WATER_H),do.points=F,verticals=T)
par(pty="s")
qqnorm(WATER_H)
qqline(WATER_H)

boxplot(EPI,DALY)

qqplot(EPI, DALY)

boxplot(EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)
qqplot(EPI, ENVHEALTH)
qqplot(EPI, ECOSYSTEM)

help(distributions)

# EXCERCISE 2
# ELand (ie Not landlocked)
EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30.,95.,1.0,prob=T))
lines(density(ELand, bw=1))
rug(ELand)

summary(ELand)
stem(ELand)

plot(ecdf(ELand),do.points=F,verticals=T)
par(pty="s")
qqnorm(ELand)
qqline(ELand)


# No_surface_water
EPI_water <- EPI[!No_surface_water]
EWater <- EPI_water[!is.na(EPI_water)]
hist(EWater)
hist(EWater, seq(30.,95.,1.0,prob=T))
lines(density(EWater, bw=1))
rug(EWater)

summary(EWater)
stem(EWater)

plot(ecdf(EWater),do.points=F,verticals=T)
par(pty="s")
qqnorm(EWater)
qqline(EWater)

# Not Desert
EPIDesert <- EPI[!Desert]
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30.,95.,1.0,prob=T))
lines(density(EDesert, bw=1))
rug(EDesert)

summary(EDesert)
stem(EDesert)

plot(ecdf(EDesert),do.points=F,verticals=T)
par(pty="s")
qqnorm(EDesert)
qqline(EDesert)

# High_Population_Density
EPIPop <- EPI[!High_Population_Density]
EPop <- EPIPop[!is.na(EPIPop)]
hist(EPop)
hist(EPop, seq(30.,95.,1.0,prob=T))
lines(density(EPop, bw=1))
rug(EPop)

summary(EPop)
stem(EPop)

plot(ecdf(EPop),do.points=F,verticals=T)
par(pty="s")
qqnorm(EPop)
qqline(EPop)

#COMPARING
boxplot(ELand, EWater, EDesert, EPop)

# Filtering EPI_regions
summary(EPI_regions)
summary(GEO_subregion)

EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia
summary(EPI_South_Asia)

EPI_Western_Europe <- EPI[GEO_subregion=="Western Europe"]
EPI_Western_Europe
summary(EPI_Western_Europe)

EPI_Sub_Saharan_Africa <- EPI[EPI_regions=="Sub-Saharan Africa"]
EPI_Sub_Saharan_Africa
summary(EPI_Sub_Saharan_Africa)

# GPW Part
# I had already input the data and named it gpw

attach(gpw)
summary(PopulationPerUnit)
summary(Area)
boxplot(PopulationPerUnit, Area)
colnames(gpw)

hist(Num.Urban.Extents)
hist(NumUnits)
plot(NumUnits, PopulationPerUnit)

## water treatment csv

water <- read.csv("water-treatment.csv")
View(water)
summary(water)
