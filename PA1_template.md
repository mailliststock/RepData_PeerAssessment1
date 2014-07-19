---
title: "Assigment 1 - Reproducible Research"
author: "Rodrigo Costa"
date: "July 19, 2014"
output: html_document
---

This is assigment 1 of the "Reproducible Research"" course.

### Loading and preprocessing the data.

1. Load the data...

```r
setwd("/Users/rcc/Desktop/coursera/Reproducible Research/a1/")
f <- read.csv("activity.csv", sep=',',header=T)
```

2. Process/transform the data...

```r
d <- na.omit(data.frame(f))
d$date = as.character(d$date)
d$date = as.Date(d$date, format = "%Y-%m-%d")
```

### What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day...

```r
h <- tapply(d$steps, d$date, sum)
hist(h, col = "blue", breaks = 15, 
     main = "Total steps per day", 
     xlab = "Steps", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Calculate mean and median total number of steps taken per day...

```r
m1 <- mean(h, na.rm = T)
m2 <- median(h, na.rm = T)
```
The mean is 1.0766 &times; 10<sup>4</sup> and the median is 10765.

### What is the average daily activity pattern?

1. Make a time series plot...

```r
p <- tapply(d$steps, d$interval, mean)
plot(p, type = "l", lwd = 2, xlab = "Interval", ylab = "Average of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

2. Which 5-minute interval contains the maximum number of steps?

```r
avg = tapply(d$steps, d$interval, mean)
ms <- names(which.max(avg))
```
The maximum of steps, on average across in the dataset, occurs at 835.

### Imputing missing values.

1. Calculate and report the total number of missing values...

```r
data_raw <- data.frame(f)
num_of_na <- as.numeric(sum(is.na(data_raw$steps)))
```
The number of NA is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset...

```r
for(i in 1:nrow(d)) {
  if(is.na(d[i,]$steps)){
    d[i,]$steps <- avg[avg$interval == d[i,]$interval, 2]
  }
}
```

3. Create a new dataset dataset but with the missing data filled in..

```r
nd <- aggregate(d$steps ~ d$date, data = d, sum,na.rm = TRUE)
```

4. Make a histogram of the total number of steps taken...

```r
hist(nd[,2], main="Number of steps each day",  breaks = 5, 
     xlab='Total Steps per day', col = "red",
     ylab='Number of days')
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels...

```r
d$day <- as.factor(ifelse(weekdays(d$date) %in% 
         c("Saturday", "Sunday"), "Weekend", "Weekday"))
summary(d$day)
```

```
## Weekday Weekend 
##   11232    4032
```

2. Make a panel plot containing a time series plot...

```r
library(lattice)
xyplot(steps ~ interval | day, aggregate(steps ~ interval + day, d, FUN = mean), 
     grid = TRUE, layout = c(1, 2), lwd =4, type = c("l"), group = day)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
