# Loading and preprocessing the data
setwd("~/Desktop/RepData_PeerAssessment1")
activity <- unzip("activity.zip")
activity <- read.csv("activity.csv")

# For this part of the assignment, you can ignore the missing values in
# the dataset.
## What is the mean total number of steps taken per day?
no_na <- activity[!(is.na(activity$steps)),] # Subset out NAs
dates <- unique(no_na$date) # Identify separate days
steps_per_day <- vector("numeric")
for (i in dates) {
  temp_date <- no_na[no_na$date==i,]
  steps_per_day <- c(steps_per_day, sum(temp_date$steps))
}
# Plot histogram
hist(steps_per_day) 
summary(steps_per_day)[3] #Median
summary(steps_per_day)[4] #Mean

## What is the average daily activity pattern?
# Make time series plot(i.e. type="1") of the 5-minute interval(x-axis) and the
# average number of steps taken, averaged across all days(y-axis).
x <- unique(activity$interval)
per_int <- vector("numeric")
for (i in x) {
  temp_int <- no_na[no_na$interval==i,]
  per_int <- c(per_int, mean(temp_int$steps))
}
plot(per_int ~ x, type=c("l"), xlab="Interval", ylab="Steps")

# Which 5-min interval, on average across all the days in the dataset, contains
# the max number of steps?
x[which(per_int %in% max(per_int))]

# Imputing missing values
# Presence of missing days may introduce bias into some calculations or summaries
# of the data.
# Calculate and report the total number rows with NAs.
length(activity[is.na(activity$steps),]$steps)

# Devise a strategy for filling in all of the missing values in the dataset.
# Create a new dataset that is equal to the original dataset but with the missing
# data filled in.
filled_NAs <- activity
filled_NAs[is.na(filled_NAs$steps),]$steps <- 0

# Make a histogram of the total number of steps taken each day. Calculate and
# report the mean and median total number of steps taken per day.
steps_per_day2 <- vector("numeric")
dates2 <- unique(filled_NAs$date)
for (i in dates2) {
  temp_date2 <- filled_NAs[filled_NAs$date==i,]
  steps_per_day2 <- c(steps_per_day2, sum(temp_date2$steps))
}
hist(steps_per_day2)

summary(steps_per_day2)[3] #Median
summary(steps_per_day2)[4] #Mean

## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of imputing missing data on the estimates of the total
## daily number of steps?
summary(steps_per_day2)[3] - summary(steps_per_day)[3] #Median diff
summary(steps_per_day2)[4] - summary(steps_per_day)[4] #Mean diff

## Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels - "weekday" and
# "weekend" indicating whether a given date is a weekday or weekend day.
weektypes<- weekdays(as.POSIXct(activity$date))
for (i in 1:length(weektypes)) {
  if (weektypes[i] == "Saturday" | weektypes[i] == "Sunday") {
    activity$weektype[i] <- "weekend"
  } else {
    activity$weektype[i] <- "weekday"
  }
}
#dim(activity[(activity$weektype=="weekday"),])
#dim(activity[(activity$weektype=="weekend"),])

# Make a panel plot containing a time series plot(i.e. type=="l") of the 5-min
# interval(x-axis) and the average number of steps taken, averaged across all
# weekday days or weekend days (y-axis). See README in the GitHub repository to
# see an example of what this plot should look like using simulated data.
no_na2 <- activity[!(is.na(activity$steps)),]
weekend_per_int <- vector("numeric")
weekday_per_int <- vector("numeric")
no_na2$int_avg <- no_na2$steps
for (i in x) {
  weekend_per_int <- c(weekend_per_int, mean(no_na2[no_na2$weektype=="weekend" & no_na2$interval==i,]$steps))
  weekday_per_int <- c(weekday_per_int, mean(no_na2[no_na2$weektype=="weekday" & no_na2$interval==i,]$steps))
}
weekend <- data.frame(per_int = weekend_per_int, weektype="weekend", interval=x)
weekday <- data.frame(per_int = weekday_per_int, weektype="weekday", interval=x)
weeks <- bind_rows(weekend, weekday)
xyplot(per_int~interval|weektype, data=weeks, type="l", 
       xlab="Interval", ylab="Average Steps-per-day")
