# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The provided activity data file should be unzipped (using the unzip command at the terminal if required) and placed in the same directory as this file.  This directory should also be set as the working directory for r.

The code below will read the CSV file and convert it into a data.table for further processing.


```r
echo = TRUE

#Libaries needed for the assignment
library(data.table)
library(ggplot2)

data <- read.csv("activity.csv")
data <- data.table(data)
```

## What is mean total number of steps taken per day?
To find these results, I removed the NAs from the data set and then aggregated the data by day for the period.  As the plot below shows, there were some days with no data (all NAs) or only zero numbers of steps.


```r
echo = TRUE

#Remove observations with NA for steps
daily_data <- data[!is.na(steps)]

#Aggregate observations by date
daily_data <- daily_data[,sum(steps),by="date"]
setnames(daily_data,"V1","steps")
daily_data <- daily_data[,date := as.Date(date)]

#Plot daily step Counts
ggplot(daily_data,aes(date,steps)) +
    geom_bar(stat="identity") +
    ggtitle("Steps per Day") + 
    xlab("Date") +
    ylab("Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#Display median and mean data
summary(daily_data[,steps])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?
The daily pattern shown below indicates a spike of activity in the morning with regular cycles throughtout the rest of the day.  The 0835 interval in the morning is the highest average performance over the course of the 2 month study.


```r
echo = TRUE

#take a mean of every five minute interval removing missing values
daily_pattern <- data[,mean(steps,na.rm = TRUE),by="interval"]
setnames(daily_pattern,"V1","steps")

ggplot(daily_pattern,aes(interval,steps)) +
    geom_line(color="blue",size=1) +
    ggtitle("Average Number of Steps per 5-minute Interval") + 
    xlab("5-minute Interval") +
    ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#Print the interval with the highest average number of steps
index <- max(daily_pattern[,steps])
daily_pattern[steps == index]
```

```
##    interval    steps
## 1:      835 206.1698
```


## Imputing missing values

```r
echo = TRUE

#Calculate the missing values in the dataset
nrow(data[is.na(steps)])
```

```
## [1] 2304
```
To impute the missing values, I replaced the NAs with the daily average for that interval over the remainder of the time period.  The plot shows that this produces a more 'complete result' as there are less missing bars in the plot. But, there are still some missing.  

These additional missing days are absent as the 'steps' value for these days was zero, not NA.  Investigating the causes behind these zeros should be a further investigation in the study.


```r
echo = TRUE

#Create a new dataset substituting interval means for NAs
modified_data <- data
index <- which(is.na(modified_data$steps))
interval_index <- modified_data$interval[index]
dp_index <- match(interval_index,daily_pattern$interval)
dp_steps <- daily_pattern$steps[dp_index]
modified_data$steps[index] <- dp_steps
modified_data <- modified_data[,date := as.Date(date)]

#plot the new data
ggplot(modified_data,aes(date,steps)) +
    geom_bar(stat="identity") +
    ggtitle("Steps per Day") + 
    xlab("Date") +
    ylab("Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
#Aggregate observations by date
modified_daily_data <- modified_data[,sum(steps),by="date"]
setnames(modified_daily_data,"V1","steps")
modified_daily_data <- modified_daily_data[,date := as.Date(date)]

#Display median and mean data
summary(modified_daily_data[,steps])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
As you can see from the mean and median data, the new data data set is consistent with the old one which is to be expected since we used the mean from each interval to populate the missing values.

## Are there differences in activity patterns between weekdays and weekends?

```r
echo = TRUE

#Prepare data for weekly pattern analysis
weekday_pattern <- modified_data[,weekday := weekdays(as.Date(date))]
weekday_pattern[weekday == "Sunday" | weekday == "Saturday",weekday := "Weekend"]
weekday_pattern[weekday != "Weekend",weekday := "Weekday"]
weekday_pattern <- weekday_pattern[,mean(steps,na.rm = TRUE),by="interval,weekday"]
setnames(weekday_pattern,"V1","steps")

#Craeate as pair of plots to compare the data
ggplot(weekday_pattern,aes(interval,steps)) +
    geom_line(color="blue",size=1) +
    ggtitle("Average Number of Steps per 5-minute Interval") + 
    xlab("5-minute Interval") +
    ylab("Steps") +
    facet_grid(weekday ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

There are some definite differences between weekends and weekdays in the dataset.  Weekends tend to have a less pronouced spike of acitivity in the morning so a more even distribution throughout the day. 
