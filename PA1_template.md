---
title: "PA1_template"
author: "N Govin"
date: "Sunday, July 19, 2015"
output:
  html_document:
    keep_md: yes
---

This is an R Markdown document.

opts_chunk$set(tidy = FALSE)

# Reproducible Research: Peer Assessment 1
# Author: NG

#### Loading and preprocessing the data

```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
library(lattice)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

#### What is mean total number of steps taken per day?
#### Lets aggregate first

```r
StepsTotal <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```
#### The Histogram is

```r
hist(StepsTotal$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

#### The mean and median are:

```r
mean(StepsTotal$steps)
```

```
## [1] 10766.19
```

```r
median(StepsTotal$steps)
```

```
## [1] 10765
```

#### What is the average daily activity pattern?
#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number 
#### of steps taken, averaged across all days (y-axis)

#### Get the mean of steps and time series plot

```r
time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
```

#### The plot is:

```r
plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
 
#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval <- which.max(time_series)
names(max_interval)
```

```
## [1] "835"
```
#### The 835th interval has maximum 206 steps.

#### Imputing missing values
#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
activity_NA <- sum(is.na(activity))
activity_NA
```

```
## [1] 2304
```
#### The total number of missing values are 2304.

#### Devise a strategy for filling in all of the missing values in the dataset. T
#### First Na is replaced by mean in 5 min interval

```r
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_activity <- activity
new_activity$steps <- fillNA
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps #### taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing #### missing data on the estimates of the total daily number of steps?

```r
StepsTotal2 <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
```

#### The Histogram is 

```r
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

#### And the mean and median are:

```r
mean(StepsTotal2$steps)
```

```
## [1] 10766.19
```

```r
median(StepsTotal2$steps)
```

```
## [1] 10766.19
```

#### Yes, these values do differ slightly.

#### Before filling the data
#####      1.Mean : 10766.19
#####      2.Median: 10765


#### After filling the data
#####      1.Mean : 10766.19
#####      2.Median: 10766.19


#### After replacing the mean is the same but the median is a little bit different
#### Are there differences in activity patterns between weekdays and weekends?
#### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

#### The weekday has the greatest peak from all steps intervals.
#### Weekends activities has more peaks over a hundred than weekday. 
