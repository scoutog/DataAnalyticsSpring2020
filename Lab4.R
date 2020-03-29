# HEAT MAP INTRO
set.seed(12345)
help(par)

par(mar = rep(0.2,4))
data_Matrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])


help(heatmap)
par(mar = rep(0.2,4))
heatmap(data_Matrix)

set.seed(678910)
for(i in 1:40){
  coin_Flip <- rbinom(1, size=1, prob = 0.5)
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each=5)
  }
}

par(mar = rep(0.2,4))
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

par(mar = rep(0.2,4))
heatmap(data_Matrix)
View(data_Matrix)
hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab="The Row Mean", ylab="Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab="Column", ylab="Column Mean", pch=19)

# TITANIC FOR RPART, CTREE, and HCLUST
library(titanic)
library(devtools)
train <- titanic_train
test <- titanic_test
summary(test)

drops <- c("Name","PassengerId", "Ticket", "Cabin")
train <- train[ , !(names(train) %in% drops)]
test <- test[ , !(names(test) %in% drops)]
View(test)
#train$Survived <- as.factor(train$Survived)
#train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
#train$SibSp <- as.factor(train$SibSp)
#train$Parch <- as.factor(train$Parch)
train$Embarked <- as.factor(train$Embarked)

#test$Survived <- as.factor(test$Survived)
#test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
#test$SibSp <- as.factor(test$SibSp)
#test$Parch <- as.factor(test$Parch)
test$Embarked <- as.factor(test$Embarked)

# RPART TITANIC
library(rpart)
model <- rpart(Survived ~ ., data = train, method="class")
plot(model)
text(model)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(model)

pred <- predict(model, test, type = "class")

# CTREE TITANIC
library(party)
library(plotmo)
ct <- ctree(Survived ~ ., data= train)
ct

plot(ct, 
     tp_args = list(fill = c("blue", "lightgray")), 
     ip_args = list(fill = c("lightgreen")))

# HCLUST TITANIC
hc <- hclust(dist(as.matrix(train)))
plot(hc)

# RANDOM FOREST
library(randomForest)
set.seed(1)

train2 <- na.omit(train)

rf <- randomForest(Survived ~ ., data=train2, ntree=1000)
varImpPlot(rf)

pred <- predict(rf, test)