#PeerAssessment1.Reproducible Research
#==================================================
setwd("~/RepData_PeerAssessment1")
activity.data <- read.csv("activity.csv")
library(timeDate)



#Getting data for number of steps per day
##############################################
daily.data <- aggregate(activity.data$steps, list(activity.data$date),sum, na.rm = T)
quartz()
hist(daily.data[,2], xlab = "Number of Steps", ylab = "Days", breaks = 10, main = "Histogram of the Number of Steps per Day",col = "RED")
with(abline(v = mean(daily.data[,2]), col = "BLACK", lwd = 4), abline(v = median(daily.data[,2]), col = "GREEN", lwd = 4))
summary(daily.data[,2])
legend("topright", lty = c(1,1), col = c("BLACK","GREEN"), lwd = c(4,4), legend = c("Mean", "Median"))

#What is the average daily pattern
##############################################
hourly.data <- aggregate(activity.data$steps, list(activity.data$interval), mean, na.rm = T)
plot(hourly.data, type = "l")
hourly.data[which.max(hourly.data$x),1]

#Replacing NAs with Mean of the time
##################################################
#Need to give whole number value to the NAs
hourly.data[,3] <- floor(hourly.data[,2])
activity.data.processed <- activity.data
for(i in 1:nrow(activity.data)){
  if(is.na(activity.data.processed[i,1])){
    activity.data.processed[i,1]<- hourly.data[hourly.data[,1]==activity.data.processed[i,3],3]
  }
}
daily.data.processed <- aggregate(activity.data.processed$steps, list(activity.data.processed$date),sum)
hist(daily.data.processed[,2], xlab = "Number of Steps", ylab = "Days", breaks = 10, main = "Histogram of the Number of Steps per Day",col = "RED")
with(abline(v = mean(daily.data.processed[,2]), col = "BLACK", lwd = 4), abline(v = median(daily.data.processed[,2]), col = "GREEN", lwd = 4))
legend("topright", lty = c(1,1), col = c("BLACK","GREEN"), lwd = c(4,4), legend = c("Mean", "Median"))
summary(daily.data.processed[,2])


#What is the difference between weekdays and weekend
##############################################################
weekday.vector<-data.frame(isWeekday(as.Date(activity.data.processed$date)))
head(weekday.vector)
activity.data.processed[,4] <- weekday.vector
names(activity.data.processed)[4] <- "is.weekday"
weekday.data <- activity.data.processed[activity.data.processed[,4]==TRUE,]
weekday.hourly.data <- aggregate(weekday.data$steps, list(weekday.data$interval), mean)
weekend.data <- activity.data.processed[activity.data.processed[,4]==FALSE,]
weekend.hourly.data <- aggregate(weekend.data$steps, list(weekend.data$interval), mean)
quartz()
plot(weekday.hourly.data, type = "l", lwd = 1, col = "RED") 
lines(weekend.hourly.data, type = "l", lwd = 1, col = "BLUE")
legend("topright", lty = c(1,1), col = c("RED","BLUE"), lwd = c(1,1), legend = c("Weekday", "Weekend"))
