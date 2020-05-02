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
##################################################
library(tidyverse)
ggplot(data=df, aes(Year, Population)) +
  geom_point() + geom_line()

library(ggplot2)
library(reshape2)
d <- melt(df, id.vars="Year")

ggplot(d, aes(Year,value, col=variable)) + 
  geom_point() + geom_line()

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
  geom_point() + geom_line() 

# Separate plots
ggplot(d_rates, aes(Year,value, col=variable)) + 
  geom_point(show.legend = FALSE) + 
  stat_smooth(show.legend = FALSE) +
  facet_wrap(~variable)

d_nums <- melt(numbers, id.vars="Year")

ggplot(d_nums, aes(Year,value, col=variable)) + 
  geom_point() + geom_line()

# Separate plots
ggplot(d_nums, aes(Year,value, col=variable)) + 
  geom_point(show.legend = FALSE) + 
  stat_smooth(show.legend = FALSE) +
  facet_wrap(~variable)

### CORRELATION
rates2 <- rates
colnames(rates2) <- c("Year","Pop","Violent","Murder","Rape","Robbery","AggAss","Prop","Burg","Larceny","Vehicle")

res <- cor(rates2)
round(res, 2)

library("Hmisc")
res2<-rcorr(as.matrix(rates2[,2:11]))

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

xyz <- cor(rates2[,2:11])
corrplot.mixed(xyz, lower.col = "black", number.cex = .9)
corrplot(xyz, order="hclust", addrect=3, col = cm.colors(100))
corrplot(xyz, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)

####################################
####### Time Series Analysis #######
####### (Not used in report) #######

library(forecast)
violent.ts <- ts(rates$Violent.Crime.rate,start = c(1997),end = c(2016),freq=1)

nValid <- 12
violent_train <- window(violent.ts, start = c(1997), end = c(2010))
violent_valid <- window(violent.ts, start = c(2010), end = c(2016))

plot(violent_train,xlab="Time",ylab="Violent Crime Rate",ylim=c(min(rates$Violent.Crime.rate), max(rates$Violent.Crime.rate)),bty="l")

## B
train.lm.season <- tslm(violent_train ~ trend, lambda=0)
train.lm.season.pred <- forecast(train.lm.season, h=6, level=0)
print(summary(train.lm.season))

plot(train.lm.season.pred,  ylab = "Violent Crime Rate", ,bty='l',xlab = "Time",xaxt="n", ylim=c(min(rates$Violent.Crime.rate), max(rates$Violent.Crime.rate)),xlim = c(1997,2016), main = "", flty = 2, col="red")
axis(1, at = seq(1997, 2016, 1)) 
lines(violent_train)
lines(train.lm.season$fitted, lwd = 2, col="blue")
lines(violent_valid)
grid()
lines(c(2010, 2010), c(min(rates$Violent.Crime.rate), max(rates$Violent.Crime.rate)),lwd=3,col="red") 
text(1998, 30000000, "Training",cex=1)
text(2002, 40000000, "Validation",cex=1)
text(1992, 65000000, "Air", cex=1.5)

accuracy(train.lm.season$fitted, violent_valid)

library(TTR)
sma_vio <- SMA(violent.ts,n=1)
plot.ts(sma_vio)
vio_fore <- HoltWinters(violent.ts, gamma=F)
plot(vio_fore)

####################################
####### Regression Analysis ########
####################################
scatter.smooth(x=rates$Year, y=rates$Violent.Crime.rate, main = "Year ~ Violent Crime Rate")
scatter.smooth(x=rates$Year, y=rates$Rape.Rate, main = "Year ~ Rape Rate")

#Box plot for outlier
par(mfrow=c(1,2))
boxplot(rates$Violent.Crime.rate, main="Violent Crime Rate")
boxplot(rates$Rape.Rate, main="Rape Rate")

# Check density plot
library(e1071)
par(mfrow=c(1, 2))  
plot(density(rates$Violent.Crime.rate), main="Density Plot: Violent Crime Rate", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(rates$Violent.Crime.rate), 2)))  
polygon(density(rates$Violent.Crime.rate), col="red")
plot(density(rates$Rape.Rate), main="Density Plot: Rape Rate", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(rates$Rape.Rate), 2))) 
polygon(density(rates$Rape.Rate), col="red")

# Correlation against time and population
cor(rates$Violent.Crime.rate, rates$Year)
cor(rates$Violent.Crime.rate, rates$Population)

cor(rates$Rape.Rate, rates$Year)
cor(rates$Rape.Rate, rates$Population)

#Model
vio.lm.fit <- lm(Violent.Crime.rate ~., data=rates)
summary(vio.lm.fit)

rape.lm.fit <- lm(Rape.Rate ~ ., data=rates)
summary(rape.lm.fit)
# Model assessment
model <- vio.lm.fit
modelSummary <- summary(model)  
modelCoeffs <- modelSummary$coefficients 
beta.estimate <- modelCoeffs["Rape.Rate", "Estimate"]  
std.error <- modelCoeffs["Rape.Rate", "Std. Error"]  
t_value <- beta.estimate/std.error  
p_value <- 2*pt(-abs(t_value), df=nrow(rates)-ncol(rates)) 
f_statistic <- model$fstatistic[1] 
f <- summary(model)$fstatistic 
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

t_value # larger t value is better
p_value # low is better
f_statistic
model_p
AIC(model)
BIC(model)

# Prediction models
set.seed(100)  
trainingRowIndex <- sample(1:nrow(rates), 0.8*nrow(rates))  
train <- rates[trainingRowIndex, ] 
test  <- rates[-trainingRowIndex, ]

vio.mod <- lm(Violent.Crime.rate ~., train)
vio.pred <- predict(vio.mod, test)
summary(vio.mod)
AIC(vio.mod)

actuals_preds <- data.frame(cbind(actuals=test$Violent.Crime.rate, predicteds=vio.pred))
correlation_accuracy <- cor(actuals_preds)  
actuals_preds

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
# => 74.86%, min_max accuracy
actuals_preds[3,1] <- 0.1
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape # 8.498%

# RAPE MODEL

rape.mod <- lm(Rape.Rate ~., train)
rape.pred <- predict(rape.mod, test)
summary(rape.mod)
AIC(rape.mod)

actuals_preds <- data.frame(cbind(actuals=test$Rape.Rate, predicteds=rape.pred))
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
# => 92.11%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape # 7.90%