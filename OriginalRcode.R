## Loading and preprocessing the data
rawData <- read.csv("E:/R/5.Reproducible/Project/activity.csv", stringsAsFactors = F)
rawData <- transform(rawData, date = as.Date(date))

## What is mean total number of steps taken per day?
# 1.Calculate the total number of steps taken per day
library(plyr)
dailyData <- ddply(rawData, ~ date, summarise, dailySteps=sum(steps))

# 2.Make a histogram of the total number of steps taken each day
hist(dailyData$dailySteps, main = "Histogram of total steps taken per day", xlab = "total steps per day")

# 3.Calculate and report the mean and median of the total number of steps taken per day
mean(dailyData$dailySteps, na.rm = T)
median(dailyData$dailySteps, na.rm = T)


## What is the average daily activity pattern?
intervalData <- ddply(rawData, ~interval, summarise, avgSteps=mean(steps, na.rm = T))

# 1.Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
with(intervalData, plot(interval, avgSteps, type = "l", main = "Average daily activity pattern", ylab = "Average steps"))

# 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervalData$interval[which.max(intervalData$avgSteps)]


## Imputing missing values
# 1.Calculate and report the total number of missing values in the dataset
sum(is.na(rawData$steps))

# 2.Strategy for filling in all of the missing values: use the mean for that 5-minute interval.
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
data <- ddply(rawData, ~ interval, transform, steps = impute.mean(steps))

# 4.Repeat 2 and 3. What is the impact of imputing missing data on the estimates of the total daily number of steps?
dailyData2 <- ddply(data, ~ date, summarise, dailySteps=sum(steps))
hist(dailyData2$dailySteps, main = "Histogram of total steps taken per day", xlab = "total steps per day")

mean(dailyData2$dailySteps, na.rm = T)
median(dailyData2$dailySteps, na.rm = T)


## Are there differences in activity patterns between weekdays and weekends?
# 1.Create a new factor variable indicating whether a given date is a weekday or weekend day.
data$weekdays <- as.factor(ifelse(weekdays(data$date) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))

# 2.Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, 
#   averaged across all weekday days or weekend days.
weekdayData <- ddply(data, ~ weekdays+interval, summarise, weekdaySteps=mean(steps))
library(lattice)
xyplot(weekdaySteps ~ interval | weekdays, type = "l", xlab = "Interval", ylab= "Number of steps", layout = c(1,2), data = weekdayData)
