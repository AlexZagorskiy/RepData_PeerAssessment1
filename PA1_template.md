------------------------------------------------------------------------

output: md\_document
--------------------

Assignemnt 1
============

Let us read the data. THe following code assumes that the csv file is
already loaded and unzipped into the working directory.

    dat<-read.csv("activity.csv")

let us calculate the total number of step per day. The Na's are just
ignored. After calculation the histogram is build, which shows the
correspoding distribution density.

    total_day<-aggregate( steps ~ date, data = dat, FUN = sum, na.rm = TRUE)
    head(total_day)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

    hist(total_day$steps, xlab="steps pers day", ylab=" Counts", col="blue", breaks=20)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Calculate the mean and median number of steps taken per day.

    steps_mean<- mean(total_day$steps)
    steps_mean

    ## [1] 10766.19

    steps_median<- median(total_day$steps)
    steps_median

    ## [1] 10765

What is mean total number of steps taken per day?
-------------------------------------------------

Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

    total_interval<-aggregate( steps ~ interval, data = dat, FUN = mean, na.rm = TRUE)
    plot(total_interval$interval, total_interval$steps, type="l", xlab="5-min interval", 
                 ylab= "Total steps", col="blue")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

max number of steps

    max_steps_int<-max(total_interval$steps)
    max_steps_int

    ## [1] 206.1698

interval containing max number of steps in average:

    #interval with max number of steps
    total_interval$interval[which(total_interval$steps==max_steps_int)]

    ## [1] 835

Imputing missing values
-----------------------

How many missing values do we have?

    steps_na<-length(dat$steps[dat$steps=="NA"])
    steps_na

    ## [1] 2304

looks like a lot!!!!

How does it look like?

    head(dat$steps, 20)

    ##  [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA

### Let us impute the missing values with average numer of steps per corresponding tiem interval.

calculating mean number of steps per interval, ignoiring missing values.
Showing first 30 intervals.

    mean_interval<-aggregate( steps ~ interval, data = dat, FUN = mean, na.rm = TRUE)
    head(mean_interval, 30)

    ##    interval     steps
    ## 1         0 1.7169811
    ## 2         5 0.3396226
    ## 3        10 0.1320755
    ## 4        15 0.1509434
    ## 5        20 0.0754717
    ## 6        25 2.0943396
    ## 7        30 0.5283019
    ## 8        35 0.8679245
    ## 9        40 0.0000000
    ## 10       45 1.4716981
    ## 11       50 0.3018868
    ## 12       55 0.1320755
    ## 13      100 0.3207547
    ## 14      105 0.6792453
    ## 15      110 0.1509434
    ## 16      115 0.3396226
    ## 17      120 0.0000000
    ## 18      125 1.1132075
    ## 19      130 1.8301887
    ## 20      135 0.1698113
    ## 21      140 0.1698113
    ## 22      145 0.3773585
    ## 23      150 0.2641509
    ## 24      155 0.0000000
    ## 25      200 0.0000000
    ## 26      205 0.0000000
    ## 27      210 1.1320755
    ## 28      215 0.0000000
    ## 29      220 0.0000000
    ## 30      225 0.1320755

Creating an impute function, which replaces the missing values with the
mean over the interval. The later are looked up in the dataframe
*mean\_interval*

    impute<-function(interval){
            i<-which(mean_interval$interval==interval) 
            mean_interval$steps[i]
    }

Let us now replace all the NAs with the corresponding means.

    dat$steps[is.na(dat$steps)]<-impute(dat$interval[is.na(dat$steps)])
    head(dat$steps, 20)

    ##  [1] 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396 0.5283019
    ##  [8] 0.8679245 0.0000000 1.4716981 0.3018868 0.1320755 0.3207547 0.6792453
    ## [15] 0.1509434 0.3396226 0.0000000 1.1132075 1.8301887 0.1698113

It looks like we've done that!!!

Now we make the same plots but with the rectified dataset. Total number
of steps per day:

    total_day_imp<-aggregate(steps ~ date, data = dat, FUN = sum, na.rm = TRUE)
    hist(total_day_imp$steps, xlab="steps pers day", ylab=" Counts", col="blue", breaks=20)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Calculate the mean and median number of steps taken per day.

    steps_mean_imp<- mean(total_day_imp$steps)
    print("mean od number of steps per day")

    ## [1] "mean od number of steps per day"

    steps_mean_imp

    ## [1] 10766.19

    steps_med_imp<- median(total_day_imp$steps)
    print("meadian of number of steps per day")

    ## [1] "meadian of number of steps per day"

    steps_med_imp

    ## [1] 10765.59

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

is it a weekday or weekend? creating a function, which returns the
answer:

    wd<-function(date){
            if(weekdays(date)=="Saturday" | weekdays(date)=="Sunday") as.factor("weekend")
            else as.factor("weekday")
    }

Creating a corresponding factor variable in the imputed dataset.

    dat$wdays<-sapply(as.Date(dat$date), wd)
    head(dat, 10)

    ##        steps       date interval   wdays
    ## 1  1.7169811 2012-10-01        0 weekday
    ## 2  0.3396226 2012-10-01        5 weekday
    ## 3  0.1320755 2012-10-01       10 weekday
    ## 4  0.1509434 2012-10-01       15 weekday
    ## 5  0.0754717 2012-10-01       20 weekday
    ## 6  2.0943396 2012-10-01       25 weekday
    ## 7  0.5283019 2012-10-01       30 weekday
    ## 8  0.8679245 2012-10-01       35 weekday
    ## 9  0.0000000 2012-10-01       40 weekday
    ## 10 1.4716981 2012-10-01       45 weekday

calculating mean number of steps per interval with the rectified
dataset. Showing first 20 intervals.

    mean_interval_imp<-aggregate( steps ~ interval + wdays, data = dat, FUN = mean, na.rm = TRUE)
    head(mean_interval_imp, 20)

    ##    interval   wdays       steps
    ## 1         0 weekday 2.317924528
    ## 2         5 weekday 0.458490566
    ## 3        10 weekday 0.178301887
    ## 4        15 weekday 0.203773585
    ## 5        20 weekday 0.101886792
    ## 6        25 weekday 1.527358491
    ## 7        30 weekday 0.713207547
    ## 8        35 weekday 1.171698113
    ## 9        40 weekday 0.000000000
    ## 10       45 weekday 1.836792453
    ## 11       50 weekday 0.407547170
    ## 12       55 weekday 0.003301887
    ## 13      100 weekday 0.433018868
    ## 14      105 weekday 0.016981132
    ## 15      110 weekday 0.203773585
    ## 16      115 weekday 0.458490566
    ## 17      120 weekday 0.000000000
    ## 18      125 weekday 1.502830189
    ## 19      130 weekday 2.270754717
    ## 20      135 weekday 0.004245283

Make a panel plot containing a time series plot (i.e.type = "l") of the
5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis)

    library(ggplot2)
    ggplot(mean_interval_imp, aes(x =interval , y=steps, color=wdays)) +
           geom_line() +
           labs(title = "Mean Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
           facet_wrap(~ wdays, ncol = 1, nrow=2)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-17-1.png)
