---
title: "PA1_template.Rmd"
author: "Amy Hays"
date: "Friday, August 15, 2014"
output: html_document
---


# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
library(reshape2)
activitydata <- read.csv("activity.csv")

## Preprocessing for Question 1
goodmelt <- melt(activitydata, id="date", measure.vars = "steps", na.rm=TRUE)
sumdata <- dcast(goodmelt, date ~ variable, sum, fill= NaN)
meandata <- dcast(goodmelt, date ~ variable, mean, fill= NaN)

## Preprocessing for Question 2

actmelt <- melt(activitydata, id="interval", 
                measure.vars = "steps", na.rm=TRUE)
actpattern <- dcast(actmelt, interval ~ variable, mean, fill= NaN)

## Preprocessing for Question 3

library(plyr)


impute.mean <- function(x) {replace(x, is.na(x), mean(x, na.rm = TRUE))}
activitydata2 <- ddply(activitydata, .(interval), transform, steps = impute.mean(steps))


newdata <- activitydata2[order(activitydata2$date), ] 

newmelt <- melt(newdata, id="date", measure.vars = "steps", na.rm=TRUE)
nsumdata <- dcast(newmelt, date ~ variable, sum, fill= NaN)
nmeandata <- dcast(newmelt, date ~ variable, mean, fill= NaN)
```

## What is mean total number of steps taken per day?

```r
hist(sumdata$steps, main = "Total Number of Steps Taken Per Day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
bmean <- mean(sumdata$steps, na.rm=TRUE)
bmedian <- median(sumdata$steps, na.rm=TRUE)

rmean <- c("Mean:", bmean)
rmedian <- c("Median:", bmedian)

noquote(rmean)
```

```
## [1] Mean:            10766.1886792453
```

```r
noquote(rmedian)
```

```
## [1] Median: 10765
```


## What is the average daily activity pattern?

```r
plot(actpattern$interval, actpattern$steps, main = "Average Daily Activity Pattern", 
    xlab = "Time Interval (5 min)", ylab = "Steps", type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


## Imputing missing values



```r
bad <- is.na(activitydata$steps)
missing <- activitydata[bad,]
count <- nrow(missing)

rcount <- c("Total Number of Missing Values:", count)
noquote(rcount)
```

```
## [1] Total Number of Missing Values: 2304
```

```r
hist(nsumdata$steps, main = "Total Number of Steps Taken Per Day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
nmean <- mean(nsumdata$steps, na.rm=TRUE)
nmedian <- median(nsumdata$steps, na.rm=TRUE)

rnmean <- c("Mean:", nmean)
rnmedian <- c("Median:", nmedian)

noquote(rnmean)
```

```
## [1] Mean:            10766.1886792453
```

```r
noquote(rnmedian)
```

```
## [1] Median:          10766.1886792453
```

### Do these values differ from the estimates from the first part of the assignment? 
 Only when it comes to the mean and median.   

###What is the impact of imputing missing data on the estimates of the total daily number of steps?
 It has made the mean and meadian the same as the mean before filling in the missing steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
mydate <- as.Date(as.character(newdata$date))
day <- weekdays(mydate)
daydata <- cbind(newdata, day)
daydata$weekend <- ifelse(daydata$day  %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"), 'weekday', 'weekend')

daydata <- transform(daydata, weekend = factor(weekend))

aggfact <- aggregate(steps ~ interval+ weekend, data = daydata, FUN = mean)



library(lattice)
xyplot(steps ~ interval|weekend, data = aggfact, layout = c(1,2), type= "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
