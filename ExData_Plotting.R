library(markdown)
library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)


if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}

data <- read.csv("activity.csv")

steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="orange", xlab="Number of Steps")


rmean <- mean(steps_by_day$steps)
rmean

rmedian <- median(steps_by_day$steps)
rmedian

steps_by_interval <- aggregate(steps ~ interval, data, mean)
par(bg = 'lightblue')
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval

incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))

imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0

steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="Green", xlab="Number of Steps")

rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)

# Calculate difference between imputed and non-imputed data

mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian

# Total difference
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)


weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")


