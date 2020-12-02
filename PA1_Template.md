# Reproducible Research:  Week#2 Project using Physiological Step Data
---
title: "RMD-document-1.rmd"
author: "Jeff Murdoch"
date: "11/5/2020"
output: html_document
---

## Loading and preprocessing the data

The physical step data will be retrieved from this location:
* https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

unzipped, and loaded into memory for analysis.

```r
# Confirm if RDS files exist.
existsRDSdirectory <- dir.exists(directory)
if( existsRDSdirectory )
{
  if( debug ) print("getRawData: Directory exists. Confirming files.")
  existsRDSfile <- file.exists(filename)
  status <- existsRDSfile
}
# Download or Access Data
if( !existsRDSfile ) 
{
  # Confirm if ZIP file exists.
  existsZIPfile <- file.exists(zipfile)
  if( existsZIPfile ) {
    if( debug ) print("getRawData: ZIP file exists. Unzipping file.")
    unzip(zipfile = zipfile)
    status <- TRUE
  } else {
    if( debug ) print("getRawData: ZIP file does not exist. Downloading file.")
    # If ZIP files do not exist, download then unzip it.
    download.file(url = zipURL, destfile = zipfile)
    existsZIPfile <- file.exists(zipfile)
    if( existsZIPfile ) {
      if( debug ) print("getRawData: ZIP file now exists. Unzipping file.")
      unzip(zipfile = zipfile)
      status <- TRUE
    } else {
      status <- FALSE
    }
  }
}
```

```
## [1] "getRawData: ZIP file does not exist. Downloading file."
## [1] "getRawData: ZIP file now exists. Unzipping file."
```

```r
if( debug ) print(paste0("getRawData: data retrieval successful? ",status))
```

```
## [1] "getRawData: data retrieval successful? TRUE"
```

```r
# Load dataSummary of Raw Data
rawdata <- read.csv(file = filename,header = TRUE)
```
## Convert the Date field from char to date and datetime.

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(stringr)
rawdata[,2] <- as.Date(rawdata[,2],"%Y-%m-%d")
rawdata[,4] <- rawdata[,2] + hours(rawdata[,3] %/% 100) + minutes(rawdata[,3] %% 100)
names(rawdata)[4] <- "datetime"
rawdata[,5] <- format(rawdata[,4],"%H:%M")
names(rawdata)[5] <- "justtime"
rawdata[,6] <- (rawdata[,3] %/% 100)*60 + rawdata[,3] %% 100
names(rawdata)[6] <- "minutes"
#head(rawdata,25)
```


## What is mean total number of steps taken per day?


```r
library(plyr)
rmnaData <- rawdata
rmnaData <- rmnaData[!is.na(rawdata[,1]),]
#head(rmnaData)
stepsPerDay <- ddply(rmnaData, .(date), summarize, sum = sum(steps))
#head(stepsPerDay)
meanStepsPerDay <- mean(stepsPerDay[,2])
medianStepsPerDay <- median(stepsPerDay[,2])
```

## What is the average daily activity pattern?

The average number of steps per day was: 10766.19

The median number of steps per day was: 10765

### Histogram of Daily Steps, ignoring empty data.

```r
par( mar = c(5,4,1,1), las = 1 )
hist(stepsPerDay[,2], breaks = 20, main = "Frequency of Daily Step Counts", xlab = "Step Counts")
```

![plot of chunk histogramOfDailyStepsIgnoreNA](figure/histogramOfDailyStepsIgnoreNA-1.png)

### Time Series of Daily Steps, ignoring empty data.
Determine the mean steps per daily increment for the overall period.

```r
stepsPerIncrementMean <- ddply(rmnaData, .(minutes), summarize, mean = mean(steps))
#head(stepsPerIncrementMean,24)
```
A plot of average activity per each 5 minute increment of the day

```r
par( mar = c(5,4,1,1), las = 1 )
plot(stepsPerIncrementMean[,1],stepsPerIncrementMean[,2],type = "l",main = "Step Counts Per Increment", xlab = "5-minute Increment", ylab = "Average Step Counts")
```

![plot of chunk timeseriesPlotOfStepsPerIncrementIgnoreNA](figure/timeseriesPlotOfStepsPerIncrementIgnoreNA-1.png)


```r
#head(stepsPerIncrementMean)
maxStepMean <- max(stepsPerIncrementMean[,2], na.rm = TRUE)
maxStepIncrement <- stepsPerIncrementMean[stepsPerIncrementMean$mean == maxStepMean,1]
maxStepPeriod <- hours(maxStepIncrement %/% 60) + minutes(maxStepIncrement %% 60)
maxStepTimeOfDay <- paste0(str_pad(maxStepPeriod$hour,2,pad = "0"),":",str_pad(maxStepPeriod$minute,2,pad = "0"))
```
The highest step count is: 206.1698

The 5-minute increment with the highest step average per day is 515 (08:35) at 206.1698 steps.


## Imputing missing values

```r
library(plyr)
library(VIM)
```

```
## Loading required package: colorspace
```

```
## Loading required package: grid
```

```
## VIM is ready to use.
```

```
## Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues
```

```
## 
## Attaching package: 'VIM'
```

```
## The following object is masked from 'package:datasets':
## 
##     sleep
```

```r
imputedData <- kNN(rawdata, variable = "steps")
#head(imputedData)
stepsPerDay <- ddply(imputedData, .(date), summarize, sum = sum(steps))
#head(stepsPerDay)
meanStepsPerDay <- mean(stepsPerDay[,2])
medianStepsPerDay <- median(stepsPerDay[,2])
```

### Basic Stats of Daily Steps, imputing missing values.

The average number of steps per day was: 9752.393

The median number of steps per day was: 10395

### Histogram of Daily Steps, imputing missing values.

```r
par( mar = c(5,4,1,1), las = 1 )
hist(stepsPerDay[,2], breaks = 20, main = "Frequency of Daily Step Counts", xlab = "Step Counts")
```

![plot of chunk histogramOfDailyStepsImputing](figure/histogramOfDailyStepsImputing-1.png)

### Time Series of Daily Steps, imputing missing values.
Determine the mean steps per daily increment for the overall period.

```r
stepsPerIncrementMean <- ddply(imputedData, .(minutes), summarize, mean = mean(steps))
#head(stepsPerIncrementMean,24)
```
A plot of average activity per each 5 minute increment of the day

```r
par( mar = c(5,4,1,1), las = 1 )
plot(stepsPerIncrementMean[,1],stepsPerIncrementMean[,2],type = "l",main = "Step Counts Per Increment, Imputing", xlab = "5-minute Increment", ylab = "Average Step Counts")
```

![plot of chunk timeseriesPlotOfStepsPerIncrementImputing](figure/timeseriesPlotOfStepsPerIncrementImputing-1.png)

Determine the maximum amount of average steps and when it occurs.

```r
#head(stepsPerIncrementMean)
maxStepMean <- max(stepsPerIncrementMean[,2], na.rm = TRUE)
maxStepIncrement <- stepsPerIncrementMean[stepsPerIncrementMean$mean == maxStepMean,1]
maxStepPeriod <- hours(maxStepIncrement %/% 60) + minutes(maxStepIncrement %% 60)
maxStepTimeOfDay <- paste0(str_pad(maxStepPeriod$hour,2,pad = "0"),":",str_pad(maxStepPeriod$minute,2,pad = "0"))
```
The highest step count is: 208.6393

The 5-minute increment with the highest step average per day is 515 (08:35) at 208.6393 steps.

## Are there differences in activity patterns between weekdays and weekends?

### Mark the step data as either weekday or weekend data.

```r
# Create a vdayType vector of weekends.
v_weekends <- c('Saturday', 'Sunday')
# Turn it into a Factor variable.
imputedData$dayType <- factor( weekdays(imputedData$date) %in% v_weekends, 
                               levels=c(TRUE, FALSE), 
                               labels=c('weekend', 'weekday'))
# Create 2 new, separate subsets to determines means on.
imputedDataWeekday <- imputedData[imputedData$dayType == "weekday",]
imputedDataWeekend <- imputedData[imputedData$dayType == "weekend",]

# Determine the means for each subset.
stepsPerIncrementMeanWeekday <- ddply(imputedDataWeekday, .(minutes), summarize, mean = mean(steps))
stepsPerIncrementMeanWeekday$dayType <- "weekday"
stepsPerIncrementMeanWeekend <- ddply(imputedDataWeekend, .(minutes), summarize, mean = mean(steps))
stepsPerIncrementMeanWeekend$dayType <- "weekend"
#names(stepsPerIncrementMeanWeekday)
#names(stepsPerIncrementMeanWeekend)
stepsPerIncrementMean <- rbind(stepsPerIncrementMeanWeekday,stepsPerIncrementMeanWeekend)
```
Plot the datasets together to show the differences in activity patterns.

```r
library(lattice)
xyplot(mean ~ minutes | dayType, data = stepsPerIncrementMean, layout = c(1,2), type = "l")
```

![plot of chunk plotStepDataBetweenWeekdaysAndWeekends](figure/plotStepDataBetweenWeekdaysAndWeekends-1.png)

### Summary
Namely, the test subjects tended to:

* get up later on the weekends

* On weekdays, do most their walking in the morning and the evening.  This pattern seems to imply a hurried morning rush, with a more relaxed return from work, with a sedentary period in the middle with the exception of perhaps meals.

* On weekends, walking was spread throughout the day, and a greater overall amount as compared to the weekdays.
