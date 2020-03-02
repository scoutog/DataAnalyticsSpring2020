# RANDOM FOREST EXAMPLE

library(randomForest)
data1 <- read.csv("car.csv", header=TRUE)
head(data1)
colnames(data1) <- c("BuyingPrice","Maintenance","NumDoors","NumPersons","BootSpace","Safety","Condition")
head(data1)
str(data1)

levels(data1$Condition)
summary(data1)

set.seed(100)
train <- sample(nrow(data1),0.7*nrow(data1),replace=FALSE)
trainSet <- data1[train,]
validSet <- data1[-train,]

summary(trainSet)
summary(validSet)

model1 <- randomForest(Condition ~ ., data=trainSet, importance=TRUE)
model1

model2<- randomForest(Condition ~ ., data=trainSet, ntree=500, mtry=6, importance=TRUE)
model2

predTrain <- predict(model2, trainSet, type="class")
table(predTrain, trainSet$Condition)

predValid <- predict(model2, validSet, type="class")
table(predValid, validSet$Condition)

importance(model2)
varImpPlot(model2)

a <- c()
i <- 5

for (i in 3:8) {
  model3 <- randomForest(Condition ~ ., data=trainSet, ntree=500, mtry = i, importance = TRUE)
  predValid <- predict(model3, validSet, type="class")
  a[i-2] <- mean(predValid == validSet$Condition)
}
a
plot(3:8,a)


library(rpart)
library(caret)
library(e1071)

model_dt <- train(Condition ~ ., data=trainSet,method="rpart")
model_dt_1 <- predict(model_dt, data=trainSet)
table(model_dt_1, trainSet$Condition)
mean(model_dt_1 == trainSet$Condition)
table(model_dt_1, trainSet$Condition)
mean(model_dt_1 == trainSet$Condition)

model_dt_vs <- predict(model_dt, newdata= validSet)
table(model_dt_vs, validSet$Condition)
mean(model_dt_vs == validSet$Condition)
