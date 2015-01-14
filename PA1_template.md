---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(data.table)
library(lubridate)
unzip("activity.zip")
data <- fread("activity.csv", stringsAsFactors = FALSE)
data$time <- substr(as.POSIXlt(sprintf("%04.0f", data$interval), 
                               format='%H%M'), 12, 16)
data$date <- ymd(data$date)
data$datetime <- ymd_hm(paste(data$date, data$time))
```




## What is mean total number of steps taken per day?


```r
library(dplyr)
sdata <- data %>%
        group_by(date) %>%
        summarise(Totalsteps = sum(steps))
```

Produce histogram of total steps    
     

```r
hist(sdata$Totalsteps, main = "Daily Steps",
     xlab = "Total Daily Steps", col = "cyan")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Find mean and median


```r
summary <- function(x) {
        funs <- c(mean, median)
        lapply(funs, function(f) f(x, na.rm = TRUE))
}

meandat <- as.data.table(summary(sdata$Totalsteps))
setnames(meandat, 1:2, c("mean", "median"))
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
