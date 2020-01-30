library(tidyverse)

multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)

mm <- lm(Homeowners~Immigrant)
mm

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm, col=2, lwd=3)

newimmigrantdata <- data.frame(Immigrant=c(0,20))
mm %>% predict(newimmigrantdata)
abline(mm)
abline(mm, col=3, lwd=3)

attributes(mm)
mm$coefficients


## Plotting, ggplot

plot(mtcars$wt, mtcars$mpg)
attach(mtcars)
library(ggplot2)
View(mtcars)
qplot(wt,mpg,data=mtcars)

ggplot(mtcars,aes(wt,mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")

qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")

ggplot(pressure, aes(temperature,pressure))+geom_line()+geom_point()

## Bar graphs
View(BOD)
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

## factor takes counts
qplot(factor(cyl),data=mtcars)
ggplot(mtcars, aes(factor(cyl)))+geom_bar()

## histograms
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10)
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=12)

qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(mpg))+geom_histogram(binwidth=4)
ggplot(mtcars, aes(mpg))+geom_histogram(binwidth=5)

## Creating boxplots
View(ToothGrowth)
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data=ToothGrowth)

qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp, len, data=ToothGrowth, geom="boxplot")

ggplot(ToothGrowth, aes(supp,len))+geom_boxplot()

qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), len, data=ToothGrowth, geom="boxplot")

qplot(interaction(supp, dose),len,data=ToothGrowth,geom="boxplot")

ggplot(ToothGrowth, aes(x=interaction(supp,dose),y=len))+geom_boxplot()

## other
ggplot(ToothGrowth, aes(supp,len,color=dose))+geom_point()

ggplot(mtcars, aes(wt,mpg,color=cyl))+geom_point()+
  geom_smooth(aes(x=wt,y=mpg), method = "lm") + 
  labs(title="Car MPG by Weight",subtitle="Author: Scout",caption="From mtcars") +
  theme_light()

ggplot(mtcars) + 
  geom_point(aes(x = hp, y = mpg, color=cyl)) + 
  facet_wrap(.~cyl) + 
  theme_light()

