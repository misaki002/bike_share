# visualization for relationship between bike rentals with other data fields 

#set directory and read csv file
setwd("D:\final\test5")
train <- read.csv("train.csv")

# numerical data
#temp, atemp, humidity, windspeed

#factorize categorical data in training set & testing set
#weather, holiday, workingday, season
train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)
#hour
train_factor$time <-substring(train$datetime,12,20)
train_factor$time <-substring(train_factor$time,1,2)
train_factor$time <-as.integer(train_factor$time)
train_factor$hour <- factor(train_factor$time)
#day
train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
#year
train_factor$year <-substring(train$datetime,1,4)
train_factor$year <-as.integer(train_factor$year)
train_factor$year <- factor(train_factor$year)

#visualization part 
library(ggplot2)
library(lubridate)
library(scales)
library(readr)
y_axis <- train$count

#Bike rentals&year
p_year <- ggplot(train, aes(x=train$datetime, y=y_axis)) + geom_point(position=position_jitter(w=0.0, h=0.4)) + xlab("Time Series") + ylab("Bike Rentals") +
          ggtitle("Overall trend of bike rentals from 2011 to 2012.\n") 
print(p_year)
ggsave("rentals_year.jpg",p_year)

#Bike rentals&hour
x_axis_hour <- train_factor$hour
p_hour <- ggplot(train, aes(x=x_axis_hour, y=y_axis)) + geom_point(position=position_jitter(w=0.0, h=0.4)) + xlab("Hour of Day") + ylab("Bike Rentals") +
     ggtitle("More bikes are rented during commuting hour.\n") 
print(p_hour)
ggsave("rentals_hour.jpg",p_hour)

#Bike rentals&temperature
x_axis <- "jitter_times"
y_axis_count <- "count"
color_temp  <- "temp_f"
train$times <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$jitter_times <- train$times+minutes(round(runif(nrow(train),min=0,max=59)))
train$temp_f <- train$temp*9/5+32
p_temp <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis_count, color=color_temp)) +
     geom_point(position=position_jitter(w=0.0, h=0.4)) +
     xlab("Hour of Day") +
     ylab("Bike Rentals") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
     ggtitle("More bikes are rented on warmer weather during weekdays.\n")+
     theme_light(base_size=25) 
print(p_temp)
ggsave("rentals_hour_temperature.png", p_temp)

#Bike rentals&humidity 
color_humi  <- "humidity"
p_humi <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis_count, color=color_humi)) +
     geom_point(position=position_jitter(w=0.0, h=0.4)) +
     xlab("Hour of Day") +
     ylab("Bike Rentals") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     scale_colour_gradientn("Humidity", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
     ggtitle("Bike rentals has different patterns related to humidity during weekdays. \n")+
     theme_light(base_size=25) 
print(p_humi)
ggsave("rentals_hour_humidity.png", p_humi)

#Bike rentals&windspeed
color_wind  <- "windspeed"
p_wind <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis_count, color=color_wind)) +
     geom_point(position=position_jitter(w=0.0, h=0.4)) +
     xlab("Hour of Day") +
     ylab("Bike Rentals") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     scale_colour_gradientn("Windspeed", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
     ggtitle("More bikes are rented when windspeed lower during weekdays. \n")+
     theme_light(base_size=25) 
print(p_wind)
ggsave("rentals_hour_wind.png", p_wind)

#Bike rentals&weekdays
train$day   <- wday(ymd_hms(train$datetime), label=TRUE)
p_weekdays <- ggplot(train, aes(x=train$times, y=y_axis, color=day)) +
     geom_smooth(fill=NA, size=2) +
     theme_light(base_size=20) +
     xlab("Hour of Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Bike Rentals") +
     scale_color_discrete("") +
     ggtitle("Bike rentals has different patterns on weekdays or weekends\n") +
     theme(plot.title=element_text(size=18))
print(p_weekdays)
ggsave("rentals_weekdays.png",p_weekdays)

#Bike rentals&season
train_factor$seasonN[train_factor$season == "1"] <- "Spring"
train_factor$seasonN[train_factor$season == "2"] <- "Summer"
train_factor$seasonN[train_factor$season == "3"] <- "Autumn"
train_factor$seasonN[train_factor$season == "4"] <- "Winter"
p_season <- ggplot(train, aes(x=train$times, y=y_axis, color=train_factor$seasonN)) +
     geom_smooth(fill=NA, size=2) +
     theme_light(base_size=20) +
     xlab("Hour of Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Bike Rentals") +
     scale_color_discrete("") +
     ggtitle("Bike rentals has different patterns on different season\n") +
     theme(plot.title=element_text(size=18))
print(p_season)
ggsave("rentals_seasons.png",p_season)

#Bike rentals&weather
train_factor$weatherN[train_factor$weather == "1"] <- "Clear"
train_factor$weatherN[train_factor$weather == "2"] <- "Cloudy"
train_factor$weatherN[train_factor$weather == "3"] <- "Light rain&snow"
train_factor$weatherN[train_factor$weather == "4"] <- "Heavy rain&snow"
p_weather <- ggplot(train, aes(x=train$times, y=y_axis, color=train_factor$weatherN)) +
     geom_smooth(fill=NA, size=2) +
     theme_light(base_size=20) +
     xlab("Hour of Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Bike Rentals") +
     scale_color_discrete("") +
     ggtitle("Bike rentals has different patterns on different weather\n") +
     theme(plot.title=element_text(size=18))
print(p_weather)
ggsave("rentals_weather.png",p_weather)

#Bike rentals&holiday
train_factor$holidayN[train_factor$holiday == "0"] <- "Non-holiday"
train_factor$holidayN[train_factor$holidy == "1"] <- "Holiday"
p_holiday <- ggplot(train, aes(x=train$times, y=y_axis, color=train_factor$holidayN)) +
     geom_smooth(fill=NA, size=2) +
     theme_light(base_size=20) +
     xlab("Hour of Day") +
     scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
     ylab("Bike Rentals") +
     scale_color_discrete("") +
     ggtitle("Bike rentals has different patterns on holiday\n") +
     theme(plot.title=element_text(size=18))
print(p_holiday)
ggsave("rentals_holiday.png",p_holiday)

#casual
ave_casual <- as.data.frame(aggregate(train[,"casual"], list(train_factor$day, train_factor$hour), mean))
ave_casual$Group.1 <- factor(ave_casual$Group.1, ordered=TRUE, labels=c("Friday", "Saturday","Sunday","Monday","Tuesday", "Wednesday","Thursday" ))
ave_casual$hour <- as.numeric(as.character(ave_casual$Group.2))
p_casual <- ggplot(ave_casual, aes(x = ave_casual$hour, y = ave_casual$Group.1)) + geom_tile(aes(fill = x)) +  xlab("Casual user distribution") + scale_fill_gradient(name="Casual", low="white", high="red") + theme(axis.title.y = element_blank())
print(p_casual)
ggsave("casual.png",p_casual)

#registered
ave_registered <- as.data.frame(aggregate(train[,"registered"], list(train_factor$day, train_factor$hour), mean))
ave_registered$Group.1 <- factor(ave_registered$Group.1, ordered=TRUE, labels=c("Friday", "Saturday","Sunday","Monday","Tuesday", "Wednesday","Thursday" ))
ave_registered$hour <- as.numeric(as.character(ave_registered$Group.2))
p_registered <- ggplot(ave_registered, aes(x = ave_registered$hour, y = ave_registered$Group.1)) + geom_tile(aes(fill = x)) + xlab("Registered user distribution") + scale_fill_gradient(name="Registered", low="white", high="red") + theme(axis.title.y = element_blank())
print(p_registered)
ggsave("registered.png",p_registered)
