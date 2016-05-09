# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

###This assignment takes data from a "personal activity monitoring device", such as a Fitbit or Jawbone, that was gathered on an anonymous individual during October and November 2012. It includes the number of steps taken in five minute intervals.
##Load Activity Data into R

```r
act_data<- read.csv("activity.csv", header = TRUE)
```
###View structure of data

```r
str(act_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

###Load dependicies

```r
library(lubridate)
library(dplyr)
library(ggplot2)
```

###Convert data into Date format

```r
act_data$date <- ymd(act_data$date)
act_data <- tbl_df(act_data)
```


## What is mean total number of steps taken per day?

```r
by_day <- act_data %>% group_by(date) %>% summarise(total_by_day = sum(steps))
by_day
```

```
## Source: local data frame [61 x 2]
## 
##          date total_by_day
##        (date)        (int)
## 1  2012-10-01           NA
## 2  2012-10-02          126
## 3  2012-10-03        11352
## 4  2012-10-04        12116
## 5  2012-10-05        13294
## 6  2012-10-06        15420
## 7  2012-10-07        11015
## 8  2012-10-08           NA
## 9  2012-10-09        12811
## 10 2012-10-10         9900
## ..        ...          ...
```

###Produce histogram of total steps per day

```r
hist(by_day$total_by_day / 1000, breaks = 15,
     col = "blue",
     xlab = "Steps Per Day (Thousand)", 
     ylab = "Number of Days",
     main = "Histogram of Total Steps
     Per Day")
```

![](PA1_Earnest_files/figure-html/unnamed-chunk-6-1.png)

###Find mean and median of total number of steps per day

```r
summary(by_day)
```

```
##       date             total_by_day  
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-31   Median :10765  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:13294  
##  Max.   :2012-11-30   Max.   :21194  
##                       NA's   :8
```


## What is the average daily activity pattern?
###Plot time series

```r
by_interval <- act_data %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm = TRUE))
## another way: by_int <- tapply(act_data$steps, act_data$interval, mean, na.rm = TRUE)
plot(avg_steps ~ interval, data = by_interval, type = "l",
     main = "Average Steps Per 5 Minute Interval Averaged Across All Days",
     xlab = "5 Minute Interval",
     ylab = "Average Steps")
     axis(side = 1, at = c(0, 500, 1000, 1500, 2000, 2355))
```

![](PA1_Earnest_files/figure-html/unnamed-chunk-8-1.png)

###Find interval with maximum average number of steps

```r
max_step_interval <- by_interval[which(by_interval$avg_steps == max(by_interval$avg_steps)),]
max_step_interval[[1]][1]
```

```
## [1] 835
```


## Imputing missing values
###Calculate total in dataset
Since ```{r, eval = FALSE}summary(act_data)``` has previously shown NAs only appear in variable "steps",

```r
sum(is.na(act_data$steps))
```

```
## [1] 2304
```

###Find and replace NA with median NA of corresponding 5 minute interval

```r
replace_na <- function(act_data){
  median_steps_interval <-
    act_data %>% group_by(interval) %>% summarise(median_steps = median(steps, na.rm = TRUE))
  act_data2 <- act_data

  for(i in 1:nrow(act_data2)){
    if(is.na(act_data2$steps[i])){
      replace_na_with_index <- which(act_data2$interval[i] == median_steps_interval$interval)
      replace_na_with <- median_steps_interval[replace_na_with_index,]
      act_data2$steps[i] <- replace_na_with$median_steps
    }
  }
  act_data2
}

## create new dataset with NAs replaced
act_data_no_na <- replace_na(act_data)
```

###Create histogram of total steps per day with NAs replaced

```r
by_day_no_na <- act_data_no_na %>% group_by(date) %>% summarise(total_by_day = sum(steps))
hist(by_day_no_na$total_by_day / 1000, breaks = 15,
     col = "red",
     xlab = "Steps Per Day (Thousand)", 
     ylab = "Number of Days",
     main = "Histogram of Total Steps
     Per Day (NAs Replaced)")
```

![](PA1_Earnest_files/figure-html/unnamed-chunk-12-1.png)

###Compare to total steps per day with NAs removed.

* Mean and median total steps per day with NAs replaced

```r
mean_steps_na_replaced <- mean(by_day_no_na$total_by_day)
mean_steps_na_replaced
```

```
## [1] 9503.869
```

```r
median_steps_na_replaced <- median(by_day_no_na$total_by_day)
median_steps_na_replaced
```

```
## [1] 10395
```

* Mean and median total steps per day with NAs removed

```r
mean_steps_no_na <- mean(by_day$total_by_day, na.rm = TRUE)
mean_steps_no_na
```

```
## [1] 10766.19
```

```r
median_steps_no_na <- median(by_day$total_by_day, na.rm = TRUE)
median_steps_no_na
```

```
## [1] 10765
```

* Difference in mean and median

```r
mean_steps_na_replaced - mean_steps_no_na
```

```
## [1] -1262.32
```

```r
median_steps_na_replaced - median_steps_no_na
```

```
## [1] -370
```

Both mean and median are decreased when NAs are replaced with median steps by interval, instead of simply removing NA values.


## Are there differences in activity patterns between weekdays and weekends?
###Create variable indicating weekday or weekend (use NA replaced dataset)

```r
act_data_no_na <- act_data_no_na %>% mutate(weekday = wday(date))
in_week <- act_data_no_na$weekday %in% c(1:5)
act_data_no_na$weekday <- factor(in_week, labels = c("weekend", "weekday"))
```

###Create time-series plot facetted by weekend/weekday of total steps by interval

```r
int_grouped_act_data_no_na <- act_data_no_na %>% group_by(interval, weekday) %>% summarise(total_steps = sum(steps))

ggplot(int_grouped_act_data_no_na, aes(interval, total_steps)) +
  geom_line() +
  facet_wrap(~weekday, ncol = 1) + 
  labs(x = "Interval", y = "Number of steps")
```

![](PA1_Earnest_files/figure-html/unnamed-chunk-17-1.png)
Yes, there appears to be higher activity level during weekdays.
