# single decision tree model for bike rentals prediction 

#set directory and read csv file
setwd("D:\final\test1")
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
install.packages('party')
library('party')

#build formula
formula_count <- count ~ hour + day + season + holiday + workingday + weather + temp +atemp + humidity + windspeed

#build model
fit.ctree <- ctree(formula_count, data=train_factor)

#variable importance
fit.ctree

#run model over testing set
predict.ctree <- predict(fit.ctree, test_factor)

#results
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)

#write results to .csv file
write.csv(submit.ctree, file="decision_tree.csv",row.names=FALSE)
