---
title: 'Peer Graded Assignment: Course Project 1'
author: Deepa
output: html_document
keep_md: yes
---


### Loading and preprocessing the data.



```r
knitr::opts_chunk$set(echo = TRUE)
Act<-read.csv("activity.csv")
```
### Calculate the total number of steps taken per day.



```r
knitr::opts_chunk$set(echo = TRUE)
totalsteps<-tapply(Act$steps,Act$date,FUN=sum,na.rm=TRUE)
```
##### Make a histogram of the total number of steps taken each day.


```r
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
qplot(totalsteps, binwidth = 1000, main = "Total number of steps taken per day",xlab="Total number of steps",ylab="Frequency")
```

![plot of chunk hist](figure/hist-1.png)

##### Calculate and report the mean and median of the total number of steps taken per day.


```r
knitr::opts_chunk$set(echo = TRUE)
mean.totalsteps<-mean(totalsteps,na.rm=TRUE)
mean.totalsteps
```

```
## [1] 9354.23
```

```r
median.totalsteps<-median(totalsteps,na.rm=TRUE)
median.totalsteps
```

```
## [1] 10395
```
### Calculate the average daily activity pattern.


##### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
averagedaily<-aggregate(x=list(steps=Act$steps),by=list(interval=Act$interval),FUN=mean,na.rm=TRUE)
ggplot(data =averagedaily,aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
    ylab("average number of steps across all days")
```

![plot of chunk time](figure/time-1.png)

##### Calculate the maximum number of steps in 5-minute interval, on average across all the days in the dataset.


```r
knitr::opts_chunk$set(echo = TRUE)
maxinterval <- averagedaily[which.max(averagedaily$steps), ]
maxinterval
```

```
##     interval    steps
## 104      835 206.1698
```

### Inputing missing values.


##### Calculate and report the total number of missing values in the dataset.


```r
knitr::opts_chunk$set(echo = TRUE)
missing.values <- is.na(Act$steps)
sum(missing.values)
```

```
## [1] 2304
```

##### Filling in all of the missing values in the dataset.

 Replace each missing value with the mean value of its 5-minute interval.


```r
knitr::opts_chunk$set(echo = TRUE)
fillingvalue<- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) 
        filled <- c(steps) else filled <- (averagedaily[averagedaily$interval == interval, "steps"])
    return(filled)
}
```

Create a new dataset that is equal to the original dataset with the missing data filled in.


```r
knitr::opts_chunk$set(echo = TRUE)
fillingdata<-Act
fillingdata$steps<- mapply(fillingvalue, fillingdata$steps, fillingdata$interval)
```

##### Make a histogram of the total number of steps taken each day.


```r
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
totalsteps <- tapply(fillingdata$steps, fillingdata$date, FUN = sum)
qplot(totalsteps, binwidth = 1000, main = "Total number of steps taken per day",xlab="Total number of steps",ylab="Frequency")
```

![plot of chunk hist1](figure/hist1-1.png)

##### Calculate and report the mean and median total number of steps taken per day.


```r
knitr::opts_chunk$set(echo = TRUE)
mean.totalsteps<-mean(totalsteps,na.rm=TRUE)
mean.totalsteps
```

```
## [1] 10766.19
```

```r
median.totalsteps<-median(totalsteps,na.rm=TRUE)
median.totalsteps
```

```
## [1] 10766.19
```
Mean and median values are higher after inputing missing data.

### Are there differences in activity patterns between weekdays and weekends?


##### Create a new factor variable in the dataset with two levels - "weekday" and "weekend".


```r
knitr::opts_chunk$set(echo = TRUE)
dayoftheweek <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}

fillingdata$date <- as.Date(fillingdata$date)
fillingdata$day <- sapply(fillingdata$date, FUN = dayoftheweek)
```

##### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
knitr::opts_chunk$set(echo = TRUE)
averagestep <- aggregate(steps ~ interval + day, data = fillingdata, base::mean)
ggplot(averagestep, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps taken")
```

![plot of chunk panel plot](figure/panel plot-1.png)





