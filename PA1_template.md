# Reproducible Research: Peer Assessment 1

### Yiming Peng - Mar 10, 2015

### 1. Loading and preprocessing the data
#### 1.1 Load the data


```r
#setwd("C:/Users/pengyi5/Downloads")
setwd("C:/Users/Yiming/Desktop/DM1/ReproduceResearch/RepData_PeerAssessment1")
#getwd()
ActivityDat <- read.csv("activity.csv", header = T)
#head(ActivityDat)
```

#### 1.2 Process/transform the data (if necessary) into a format suitable for the analysis. 


```r
library(plyr)
#levels(ActivityDat$date)
TransDatByDate <- ddply(ActivityDat, .(date), summarize, freq=length(steps), 
                        totmiss = sum(is.na(steps)), totsteps = sum(steps))
```

### 2. What is mean total number of steps taken per day?

#### For this part of the assignment, you can ignore the missing values in the dataset.

#### 2.1 Calculate the total number of steps taken per day

```r
# TransDatByDate[,c(1,4)]
```

#### Comment: Already calculated above

#### 2.2 Make a histogram of the total number of steps taken each day (If you do not understand the difference between a histogram and a barplot, research the difference between them.)

```r
hist(TransDatByDate$totsteps,breaks=20, ylim = c(0,19),
     main = "Histogram of total steps each day - with missing")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

#### 2.3 Calculate and report the mean and median of the total number of steps taken per day

```r
mean(TransDatByDate$totsteps, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(TransDatByDate$totsteps, na.rm=T)
```

```
## [1] 10765
```

### 3. What is the average daily activity pattern?

#### 3.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
TransDatByInterval <- ddply(ActivityDat, .(interval), summarize, freq=length(steps), 
                        totmiss = sum(is.na(steps)), totsteps = sum(steps, na.rm=T), 
                            meansteps = mean(steps, na.rm=T))
#head(TransDatByInterval)
plot(meansteps~interval, data=TransDatByInterval, type="l", 
     ylab = "Avg steps over days", xlab = "Interval", main = "Time series plot")
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

#### 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
TransDatByInterval[which(TransDatByInterval$meansteps==max(TransDatByInterval$meansteps)),]$interval
```

```
## [1] 835
```

### 4. Imputing missing values

#### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#### 4.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(ActivityDat$steps))
```

```
## [1] 2304
```

#### 4.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#### Comment: It looks that using the mean/median for each day to impute missing value won't work, as for each day it's either all missing or non-missing. So we will use the mean for each 5-minute interval to impute missing values. 

#### 4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# First we merge the original data with transformed by interval data. The new data set "ActivityDatNewMis" still contains missing values. 

ActivityDatNewMis <- merge(ActivityDat, TransDatByInterval,by="interval")
# head(ActivityDatNewMis)
# sum(ActivityDat$steps==0,na.rm=T)
# sum(ActivityDatNewMis$totsteps==0,na.rm=T)
# sum((ActivityDatNewMis$steps==0),na.rm=T)
# sum(is.na(ActivityDatNewMis$steps))
# head(ActivityDatNewMis[(is.na(ActivityDatNewMis$steps)),])
# sub=subset(ActivityDatNewMis,is.na(ActivityDatNewMis$steps))
# sum(sub$totsteps==0)

# Then we create a new data set "ActivityDatNew" which set missing value as that 5 minute interval's mean
ActivityDatNew <- ActivityDatNewMis
ActivityDatNew[(is.na(ActivityDatNew$steps)),]$steps = 
  ActivityDatNew[(is.na(ActivityDatNew$steps)),]$meansteps
# head(ActivityDatNew)
# sum(is.na(ActivityDatNew$steps))
# sum((ActivityDatNew$steps==0),na.rm=T)
```

#### 4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
TransDatByDateNew <- ddply(ActivityDatNew, .(date), summarize, freq=length(steps), 
                        totmiss = sum(is.na(steps)), totsteps = sum(steps))
#head(TransDatByDateNew)
hist(TransDatByDateNew$totsteps,breaks=20, ylim = c(0,19), 
     main = "Histogram of total steps each day - missing imputed")
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(TransDatByDateNew$totsteps)
```

```
## [1] 10766.19
```

```r
median(TransDatByDateNew$totsteps)
```

```
## [1] 10766.19
```

#### Answer: From the result we can see that mean stays at the same value but median slightly changed. Total daily number of steps after imputing missing data should be no smaller than that before imputing, because "NA" has been changed to non-negative values. 


### 5. Are there differences in activity patterns between weekdays and weekends?

#### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#### 5.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
ActivityDatNewWeek <- ActivityDatNew
# head(ActivityDatNewWeek)
ActivityDatNewWeek$Weekday <- weekdays(as.Date(ActivityDatNewWeek$date))
# head(ActivityDatNewWeek)
ActivityDatNewWeek$WeekdaySim <- "weekday"
ActivityDatNewWeek[(ActivityDatNewWeek$Weekday %in% c("Saturday", "Sunday")),]$WeekdaySim <- "weekend"
# head(ActivityDatNewWeek)
```

#### 5.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
TransDatByIntervalWeek <- ddply(ActivityDatNewWeek, .(interval,WeekdaySim), 
                                summarize, freq=length(steps), totsteps = sum(steps),
                                meansteps = mean(steps))
# head(TransDatByIntervalWeek)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.0.3
```

```r
xyplot(meansteps~interval | WeekdaySim, data=TransDatByIntervalWeek, type="l",
       layout = c(1,2), ylab = "Avg steps over days", xlab = "Interval")
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

#### Comment: It is interesting to see that during weekday, 8:35 am has the highest steps. While during the weekend, the steps are relatively spread over the day. 

