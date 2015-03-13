Assignment 1 - Analysing Step Count Data
========================================

By: Peter Stathakos  
2015-03-12

**Abstract**: This is assignment 1 for the course Reproducible Research.  
The purpose of this assignment is to analyse a given set of step count data using R markdown and the knitr package in R.  


####Loading and Processing the Data  
Load the data into a data frame and transform the data as required for the analysis.  
```{r loaddata, echo = TRUE}

    library(plyr)
    library(dplyr)
    library(ggplot2)

    ## Remove scientific notation
    options(scipen = 999)
    
    stepdatafile <- paste(getwd(), "/activity.csv", sep="")
    stepdata <- read.csv(stepdatafile, header = TRUE, na.strings = "NA")
    
    ## Subset for all lines with no NA values
    stepdatasub <- subset(stepdata, steps != "NA")
        
    ## Group data and summarize by day
    byday <- group_by(stepdatasub, date)
    stepsum <- ddply(byday, "date", summarize, totalsteps = sum(steps))
 
```


####Plot Daily Steps and Find the Mean and Median Steps Per Day  
Calculate the total, mean, and median steps for each day.  
```{r averagesteps, echo = TRUE}
    ## Plot the histogram of the steps taken per day
    hist(stepsum$totalsteps, col = "blue4", main = "Steps Per Day", xlab = "Total Steps")

    ##  Sort stepsum to obtain the median
    stepsumsorted <- sort(stepsum$totalsteps)
        
    ## Calculate the mean and median steps by day
    stepmeancalc <- mean(stepsum$totalsteps)
    
    ## Round mean steps result
    stepmeancalc <- format(round(stepmeancalc, 2), nsmall = 2)
    
    stepmediancalc <- median(stepsumsorted)

```
The mean of the steps per day is `r stepmeancalc`, and the median of the steps per day is `r stepmediancalc`.  



####Average Daily Activity Pattern  
Plot the daily activity pattern.  
```{r plotdata1, echo = TRUE}

    ## Calculate average setps by interval
    intervalmean <- ddply(stepdatasub, "interval", summarize, stepmean = mean(steps))
    
    ## Plot steps by interval
    ggplot(data = intervalmean, aes(x=interval, y=stepmean)) + geom_point() +
        xlab("Interval") +
        ylab("Mean Steps") +
        ggtitle("Average Daily Steps by 5 Minute Interval")

    ## Find interval with the maximum mean steps
    intervalmeansorted <- arrange(intervalmean, desc(stepmean))
    maxmeanstepinterval <- intervalmeansorted[1,1]

```
The interval with the maximum number of steps across all of the days is `r maxmeanstepinterval`.



####Inputting Missing Values  
Since the original data set was missing some step values, extrapolate for the missing data.    
Then determine how this extrapolated data affects the calculations which were done above.  
```{r missingdata, echo = TRUE}

    ## Calculate to total number of NA rows in the initial dataset
    nasub <- subset(stepdata, is.na(steps))
    nacount <- nrow(nasub)

```
The total number of missing values (NA values) in the original dataset was `r nacount`. 

```{r extrapolatemissingdata}
    ## Calculate the mean value for each interval
    intervalmean <- ddply(stepdatasub, "interval", summarize, stepmean = mean(steps))
    
    ## Merge the interval mean with the orignal data set to obtain the mean values by interval
    stepdataex <- merge(stepdata, intervalmean, by = "interval")
    
    ## Fill in the NA values with the interval mean values
    stepdataex$steps[is.na(stepdataex$steps)] <- stepdataex$stepmean[is.na(stepdataex$steps)]
    
    ## Subset dataset to be equivalent to original dataset
    stepdataex <- subset(stepdataex, select = c(steps, date, interval))
        
    
    ## Determine mean and median for new data set
    ## Group data and summarize by day
    bydayex <- group_by(stepdataex, date)
    stepsumex <- ddply(bydayex, "date", summarize, totalsteps = sum(steps))
        
        
    ## Plot histogram of the total steps by day
    hist(stepsumex$totalsteps, col = "blue4", main = "Steps Per Day - Extrapolated For Missing Data",
         xlab = "Total Steps")
        
    ##  Sort stepsum to obtain the median
    stepsumexsorted <- sort(stepsumex$totalsteps)
        
    ## Calculate the mean and median steps by day
    stepmeanex <- mean(stepsumex$totalsteps)
    stepmedianex <- median(stepsumexsorted)

    ## Round mean and median steps result
    stepmeanex <- format(round(stepmeanex, 2), nsmall = 2)
    stepmedianex <- format(round(stepmedianex, 2), nsmall = 2)

```
After the missing data was extrpolated, the mean of the steps per day is `r stepmeanex`, and the median of the steps per day is `r stepmedianex`.  



####Differences in Activity Patterns  
Compare the step activity for weekdays and weekends and comapre the steps for each.  
```{r plotdata2, echo = TRUE}
 
    ## Add a factor varaible for the day of the week to the data set
    stepdataex <- data.frame(stepdataex, c("NA"))
    colnames(stepdataex) <- c("steps", "date", "interval", "weekday")
    
    ## Assign day names for each date
    stepdataex$weekday <- factor(weekdays(as.Date(stepdataex$date, format="%Y-%m-%d")))
    
    ## Create levels for weekday/weekend
    levels(stepdataex$weekday) <- list(weekday = 
        c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
        weekend = c("Saturday", "Sunday"))
        
    ## Plot activity based on weekday/weekend
    qplot(interval, steps, data = stepdataex, 
        geom = c("line"), type = "l", facets = weekday~.) + 
        ylab("Steps") +
        xlab("Interval") + 
        ggtitle("Number of Steps per Interval - Weekdays vs. Weekends") 
    
```
