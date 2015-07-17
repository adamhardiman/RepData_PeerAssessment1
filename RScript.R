#PeerAssessment1.Reproducible Research
#==================================================

#Question 1
setwd("~/RepData_PeerAssessment1")
activity.data <- read.csv("activity.csv")
#getting data for sum of 
daily.data <- aggregate(activity.data$steps, list(activity.data$date),sum, na.rm = T)
quartz()
hist(daily.data[,2], xlab = "Number of Steps", ylab = "Days", breaks = 10, main = "Histogram of the Number of Steps per Day",col = "RED")
with(abline(v = mean(daily.data[,2]), col = "BLACK", lwd = 4), abline(v = median(daily.data[,2]), col = "GREEN", lwd = 4))
summary(daily.data[,2])
legend("topright", lty = c(1,1), col = c("BLACK","GREEN"), lwd = c(4,4), legend = c("Mean", "Median"))

#What is the average daily pattern
hourly.data <- aggregate(activity.data$steps, list(activity.data$interval), mean, na.rm = T)
plot(hourly.data, type = "l")
hourly.data[which.max(hourly.data$x),1]

#What is the difference between weekdays and weekend
weekdays(as.Date(activity.data$date))