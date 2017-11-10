# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
1. Load the data

```r
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
data <- transform(data, date=as.Date(as.character(date), "%Y-%m-%d"))
```

```
## Warning in strptime(x, format, tz = "GMT"): unknown timezone 'default/Asia/
## Tokyo'
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
total_step = tapply(data$step, data$date, sum)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(total_step, main="Histogram of total number of steps per day", xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(total_step, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total_step, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval, steps_by_interval$steps, type="l", main="Average number of steps over all days", xlab="Interval", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_by_interval[which.max(steps_by_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Note that I use the mean for that 5-minute interval for imputing.

```r
imputed_data <- data
for(i in 1:nrow(imputed_data)){
    if (is.na(imputed_data$steps[i])){
        imputed_data$steps[i] <- steps_by_interval$steps[steps_by_interval$interval == imputed_data$interval[i]]
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imputed_total_step = tapply(imputed_data$step, imputed_data$date, sum)
hist(imputed_total_step, main="Histogram of total number of steps per day (imputed)", xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(imputed_total_step, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(imputed_total_step, na.rm=TRUE)
```

```
## [1] 10766.19
```
The shape of the histogram remains the same but the frequency increased. Also, the median is also slightly decreased because of the impact of imputing missing data.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday"" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
imputed_data$type_of_day <- weekdays(imputed_data$date)
imputed_data$type_of_day <- as.factor(ifelse(weekdays(imputed_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(ggplot2)
imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, imputed_data, mean)
qplot(interval, steps, data = imputed_steps_by_interval, facets = type_of_day~., geom=c("line"), xlab="Interval", ylab="Number of steps") + facet_wrap(~ type_of_day, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
