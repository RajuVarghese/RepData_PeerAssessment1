# Reproducible Research: Peer Assessment 1

This document is the submission for Peer Assignment 1 of the Coursera Data Science course *Reproducible Research*. It was forked from the GitHub repository [RepData_PeerAssessment1](https://github.com/rdpeng/RepData_PeerAssessment1) and extended as specified in the [exercise description](https://class.coursera.org/repdata-011/human_grading/view/courses/973512/assessments/3/submissions). 

## Loading and preprocessing the data

Load the data in a CSV file wrapped up in a zip file. As the date column will be a factor convert it to a POSIX date field. The first few rows of the data frame are displayed for sanity checking.


```r
# read the CSV file within the ZIP file
activity <- read.csv (unz ("activity.zip", "activity.csv"))

# convert the date to a POSIX date column
activity$date <- as.POSIXct (activity$date)

# examine the first few rows
head (activity)
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

## What is mean total number of steps taken per day?

I am using Hadley Wickham's library *dplyr* to massage the activity data into the required form. The NA measurements are filtered out and the steps are summed up for each day. A histogram of the steps per day (bin width of 5000 steps) shows the distribution.


```r
library(ggplot2)
suppressPackageStartupMessages (library (dplyr, quietly=TRUE))

# use dplyr's pipe operator to filter and summarize
stepsByDate <-
  activity                      %>% # from activity ...
  filter (!is.na (steps))       %>% # take only valid step counts ...
  group_by (date)               %>% # group it by the date ...
  summarize (nSteps=sum (steps))    # and total up the steps

# plot a histogram of the steps
qplot (stepsByDate$nSteps,
       xlab="Distribution of steps per day", ylab="Count",
       binwidth=5000)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean and median values of the steps per day are almost identical.


```r
# write out the mean of the steps
mean (stepsByDate$nSteps)
```

```
## [1] 10766.19
```

```r
# write the median of the steps
median (stepsByDate$nSteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

The time series plot of the average step count by interval showing the activity over a 24-hour period. One can discern the diurnal cycle: sleep, followed by high activity in the morning, less intense movement in the afternoon and a quietening down towards the evening. Though not asked for the average steps over the whole day is shown with a horizontal yellow line. One can see that at the ends of the plot the activity level is well below the average and in the middle most of the data points are above the average line.


```r
meanStepsByInterval <-
  activity                      %>% # from activity ...
  filter (!is.na (steps))       %>% # take only the valid step counts ...
  group_by (interval)           %>% # group it by the interval ...
  summarize (nSteps=mean (steps))   # and calculate the average #steps

qplot (meanStepsByInterval$interval,
       meanStepsByInterval$nSteps,
       xlab="Interval",
       ylab="Average number of steps",
       geom="line") +
   geom_hline (aes (yintercept=mean (meanStepsByInterval$nSteps)),
               color="yellow",
               alpha=0.8)
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The most active interval (ie the 5-minute period with the most number of steps) is shown together the highest step count.



```r
highestStepCount <- max (meanStepsByInterval$nSteps)
meanStepsByInterval [meanStepsByInterval$nSteps == highestStepCount, ]
```

```
## Source: local data frame [1 x 2]
## 
##   interval   nSteps
## 1      835 206.1698
```

## Imputing missing values

The total number of missing values for the steps field:


```r
sum (is.na (activity$steps))
```

```
## [1] 2304
```

The strategy chosen for imputing missing values is to replace NA's with the average number of steps for that time interval.


```r
# make a new table from the original activity with a new field called nSteps that
# contains the average of that interval
cleanedActivity <- merge (meanStepsByInterval, activity, by="interval")
cleanedActivity$steps <-
  ifelse (is.na (cleanedActivity$steps),
          round (cleanedActivity$nSteps),
          cleanedActivity$steps)

# as done earlier, total the steps of each day
stepsByDate <-
  cleanedActivity               %>% # from activity with imputed missing data ...
  group_by (date)               %>% # group it by the date ...
  summarize (nSteps=sum (steps))    # and total up the steps

# plot a histogram of the steps
qplot(stepsByDate$nSteps,
      xlab="Distribution of steps per day with imputed missing values",
      ylab="Count", binwidth=5000)
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
# write out the new mean of the steps after cleaning the data
mean (stepsByDate$nSteps)
```

```
## [1] 10765.64
```

```r
# the median of the steps after cleaning the data
median (stepsByDate$nSteps)
```

```
## [1] 10762
```

As one can see, there is a small change in the median after imputing missing values; the mean is almost the same as before.

From the histogram one can see that total daily number of steps has increased when one compares it with the previous one.


## Are there differences in activity patterns between weekdays and weekends?

The date is used to ascertain if it was a weekeday or a weekend and based on this factor two plots are made with the enhanced data.


```r
# this function returns either "weekday" or "weekend" depending on the date given to it.
weekdayType <- function (x) {
  listOfWeekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  ifelse (weekdays (x) %in% listOfWeekdays, "weekday", "weekend")
}

# add a new column for the weekday type
cleanedActivity$wd <- as.factor (weekdayType (cleanedActivity$date))

# calculate the mean number of steps in each interval for weekdays and weekends
meanStepsByInterval <-
  cleanedActivity               %>% # from activity with imputed missing data ...
  group_by (interval, wd)       %>% # group it by the date ...
  summarize (nSteps=mean (steps))   # and get the average #steps

# plot a faceted graph of the mean number of steps
qplot (interval, nSteps, data=meanStepsByInterval,
       xlab="Interval", ylab="Average number of steps",
       geom="line",
       facets=wd ~ .) 
```

![](./PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


The two plots show that the weekday and weekend activities are noticeably  different.
