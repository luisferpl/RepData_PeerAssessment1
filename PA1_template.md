---
title: "Assigment Course Project 1"
author: "Luis Paz"
date: "7 May 2016"
output: html_document
---

```{r setup, include=FALSE}
library(dplyr)
knitr::opts_chunk$set(echo = TRUE)
```

##Loading and preprocessing the data
        
1. Load the data (i.e. read.csv())
        
```{r echo = TRUE}
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
file <- unzip(temp)
unlink(temp)
act <- read.csv(file)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
        
```{r echo = TRUE}
data <- na.omit(act)
data$date <- as.Date(data$date)
```
        
## What is mean total number of steps taken per day?
        
1. Calculate the total number of steps taken per day

```{r echo = TRUE}
steps_day <- data %>% group_by(date) %>% summarise(tot_steps = sum(steps))
```
        
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
        
```{r echo = TRUE}
hist(steps_day$tot_steps,
xlab = "Total number of steps taken each day",
ylab = "Count",
main = "Histogram",
col = 3)
```
        
3. Calculate and report the mean and median of the total number of steps taken per day
        
```{r echo = TRUE}
mean_steps <- mean(steps_day$tot_steps)
median_steps <- median(steps_day$tot_steps)
```

The mean of the total number of steps taken per day is `r mean_steps`.
The median of the total number of steps taken per day is `r median_steps`.
        
## What is the average daily activity pattern?
        
1. Make a time series plot (i.e. type = "l") of the 5-minute 
interval (x-axis) and the average number of steps taken, averaged 
across all days (y-axis)
        
```{r echo = TRUE}
five_min_intr <- data %>%
group_by (interval) %>%
summarise(avg_steps=mean(steps))
plot(five_min_intr$interval,
five_min_intr$avg_steps, 
type ="l", 
xlab = "Interval",
ylab = "Average number of steps taken",
main = "Time series of the 5-minutes interval")
```
        
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        
```{r echo = TRUE}
max_step_interval <- five_min_intr$interval[which.max(five_min_intr$avg_steps)]
```

The maximum number of steps of the 5-minute interval, on average across all the days in the dataset happened during interval `r max_step_interval`.
        
## Imputing missing values
        
1. Calculate and report the total number of missing values in the 
dataset (i.e. the total number of rows with NAs)
        
```{r echo = TRUE}
missing_val <- sum(is.na(act))
```

The total number of missin values in the dataset is `r missing_val`.   
        
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
        
```{r echo = TRUE}
fill_nas <- act
for (i in 1:nrow(fill_nas)) {
if (is.na(fill_nas$steps[i])) {
x <- which(fill_nas$interval[i]==five_min_intr$interval)
fill_nas$steps[i] <- five_min_intr[x,]$avg_steps
}
}
```
        
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the 
assignment? What is the impact of imputing missing data on the estimates 
of the total daily number of steps?
        
```{r echo = TRUE}
fill_day <- fill_nas %>%
group_by (date) %>%
summarise(tot_steps=sum(steps))
hist(fill_day$tot_steps,
xlab = "Total number of steps taken each day",
ylab = "Count",
main = "Histogram",
col = 3)
fill_mean_steps <- mean(fill_day$tot_steps)
fill_median_steps <- median(fill_day$tot_steps)
```

The mean of the total number of steps taken per day is `r fill_mean_steps`.
The median of the total number of steps taken per day is `r fill_median_steps`.
        
## Are there differences in activity patterns between weekdays and weekends?
        
1. Create a new factor variable in the dataset with two levels - 
"weekday" and "weekend" indicating whether a given date is a weekday 
or weekend day.
        
```{r echo = TRUE}
fill_nas$weekday <- weekdays(as.Date(fill_nas$date))
fill_nas$day_type[fill_nas$weekday %in% c("Saturday","Sunday")] <- "weekend"
fill_nas$day_type[!(fill_nas$weekday %in% c("Saturday","Sunday"))] <- "weekday"
```
        
2. Make a panel plot containing a time series plot (i.e. type = "l") of 
the 5-minute interval (x-axis) and the average number of steps taken,         averaged across all weekday days or weekend days (y-axis). See the            README file in the GitHub repository to see an example of what this           plot should look like using simulated data.

```{r echo = TRUE}
five_min_avg <- fill_nas %>%
group_by (day_type, interval) %>%
summarise(avg_steps=mean(steps))
```

```{r echo = TRUE}
library(ggplot2)

qplot(interval,
avg_steps,
data = five_min_avg,
geom = "line",
xlab = "Interval",
ylab = "Number of steps",
main = "Average number of steps taken in Weekdays and Weekends",
facets = day_type ~ .)
```