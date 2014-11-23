BIKE-SHARING-DEMAND-
====================
## Bike data testing & training ###

setwd("C:\\Users\\Nitesh Kumar\\Documents\\Kaggle\\Bike Sharing")


library (lubridate)
library(ggplot2)
library(caret)
library (rpart)

# Read data # 

sample<-read.csv("F:/BIKE DATA/sampleSubmission.csv")
train<-read.csv("F:/BIKE DATA/train.csv")
test<-read.csv("F:/BIKE DATA/test.csv")
names(train)


# convert variable in format i.e (cleaning the data) # 

train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
str(train)
train$hour <- hour(train$datetime)
train$datetime <- as.Date(train$datetime)
train$weekday <- weekdays(train$datetime)
train$hour <- as.factor(train$hour)
train$weekday <- as.factor(train$weekday)
train$month <- month(train$datetime)
train$month <- as.factor(train$month)
train <- train[,-c(1,10,11)]

##partitioning the data ##

set.seed(101)
intrain <- createDataPartition(train$count, p=0.7, list = F, times = 1)
validation <- train[-intrain,]
train <- train[intrain,]

## formula for model training ##

n <- names(train)
f <- as.formula(paste("count ~", paste(n[!n %in% "count"], collapse = " + ")))

## Algoritham for model training ##

fit<-rpart(f,method="anova",data=train)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit,uniform=TRUE)
text(fit,use.n=TRUE,all=TRUE,cex=1)
pfit<-prune(fit,cp=0.01160389)
plot(pfit,unifrom=TRUE)
text(pfit,use.n=TRUE,all=TRUE,cex=1)

## tree is ready now use this model for prediction .

predictrpart<-predict(fit,validation,type="vector")
errorrpart<-sqrt(mean((predictrpart - validation$count)^2))
denom<-sqrt(mean((validation$count-mean(validation$count))^2))
relativeerror <- errorrpart/denom


### testing the test data ##

test$season <- as.factor(test$season)
test$holiday <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)
str(test)
test$hour <- hour(test$datetime)
test$datetime <- as.Date(test$datetime)
test$weekday <- weekdays(test$datetime)
test$hour <- as.factor(test$hour)
test$weekday <- as.factor(test$weekday)
test$month <- month(test$datetime)
test$month <- as.factor(test$month)
test <- test[,-c(1)]
predictrparttest<-predict(fit,test,type="vector")
sam$count <- predictrparttest
am$count <- as.integer(sam$count)
sam$datetime <- as.character(sam$datetime)
write.csv(sam, file = "rf.csv", row.names = F)
