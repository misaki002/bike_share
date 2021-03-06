# random forest model for bike rentals prediction

#set directory and read csv file
setwd("D:\final\test3")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# numerical data
#temp, atemp, humidity, windspeed

#factorize categorical data in training set & testing set
#weather, holiday, workingday, season
train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)
test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(test$season)

#create hour factors from timestamp
#hour
train_factor$time <-substring(train$datetime,12,20)
test_factor$time <- substring(test$datetime,12,20)
train_factor$time <-substring(train_factor$time,1,2)
test_factor$time <-substring(test_factor$time,1,2)
train_factor$time <-as.integer(train_factor$time)
test_factor$time <-as.integer(test_factor$time)
train_factor$hour <- factor(train_factor$time)
test_factor$hour <- factor(test_factor$time)

#create day of week factors from timestamp
#day
train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

#create sunday factors (feature engineering)
#sunday
train_factor$sunday[train_factor$day == "Sunday"] <- "1"
train_factor$sunday[train_factor$day != "Sunday"] <- "0"
test_factor$sunday[test_factor$day == "Sunday"] <- "1"
test_factor$sunday[test_factor$day != "Sunday"] <- "0"
train_factor$sunday <- as.factor(train_factor$sunday)
test_factor$sunday <- as.factor(test_factor$sunday)

#create year factors (feature engineering)
#year
train_factor$year <-substring(train$datetime,1,4)
test_factor$year <- substring(test$datetime,1,4)
train_factor$year <-as.integer(train_factor$year)
test_factor$year <-as.integer(test_factor$year)
train_factor$year <- factor(train_factor$year)
test_factor$year <- factor(test_factor$year)

#install party package
install.packages('randomForest')
library(randomForest)

#variables setting
myNtree = 500
myMtry = 5
myImportance = TRUE
set.seed(415)

#build formula&model for "casual"
casualFit <- randomForest(casual ~ hour + year + humidity + temp + atemp + workingday + weekday, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
test$casual <- predict(casualFit, test)

#build formula&model for "registered"
registeredFit <- randomForest(registered ~ hour + year + season + weather + workingday + humidity + weekday + atemp, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
test$registered <- predict(registeredFit, test)

#results
test$count <- round(test$casual + test$registered, 0)

#plot testing
plot(train$count)
plot(test$count)

#write results to .csv file
submit <- data.frame (datetime = test$datetime, count = test$count)
write.csv(submit, file = "randomForest.csv", row.names=FALSE)
