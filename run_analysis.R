# Reproducible Research Week2 Project 1
# Mohsen Azimi, Aug, 2020

# Clear variables
rm(list=ls()) 

# Load packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(httpuv)

# 1. Reading in the dataset and/or processing the data
if (!file.exists("activity.csv")) { 
  unzip("activity.zip") 
}

activity0 <- read.csv("activity.csv", header=TRUE, na.strings = "NA")

# Remove NA
activity <- na.omit(activity0)


# Check
summary(activity)
str(activity)
head(activity, 3)
tail(activity, 3)

# 2. Histogram of the total number of steps/day
# 3. Mean and median number of steps/day
act <- summarize(group_by(activity,date),daily.step=sum(steps))

# Mean total number of steps/day
mean.act <- as.integer(mean(act$daily.step))
sprintf("Mean number of steps/day = %g", mean.act)

# Median total number of steps/day
median.act <- as.integer(median(act$daily.step))
sprintf("Median number of steps/day = %g", median.act)

# Plot histogram
plot.steps.day <- ggplot(act, aes(x=daily.step)) + 
  geom_histogram(binwidth = 2000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.act, colour="red",  size=2) +
  geom_vline(xintercept=median.act, colour="green" , linetype="dashed", size=2) +
  labs(title="Histogram of number of steps/day", y="Frequency", x="Steps/day") 
plot.steps.day

# 4. Time series plot of the average number of steps taken
act <- activity %>% group_by(interval) %>% summarize(mean.step=mean(steps))

plot.step.interval <- ggplot(act, aes(x=interval,y=mean.step)) + 
  geom_line(color="blue",size=1.5) + 
  labs(title="Average number of steps", y="Average number of steps", x="Times (x 5 minutes)")
plot.step.interval


# 5. The 5-minute interval that, on average, contains the maximum number of steps
optimal <- which.max(act$mean.step)
optimal.step <- act$interval[optimal]
sprintf("Maximum number of steps @  %gth 5-min interval", optimal.step)


# 6. Code to describe and show a strategy for imputing missing data
# a-Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity0)) 
# b- Devise a strategy for filling in all of the missing values in the dataset. 
#   The strategy does not need to be sophisticated. For example, you could use the 
#   mean/median for that day, or the mean for that 5-minute interval, etc.
impute.act <- activity0
impute.act$steps[is.na(impute.act$steps)] <- mean(impute.act$steps,na.rm=TRUE)
impute.act$steps <- as.numeric(impute.act$steps)
impute.act$interval <- as.numeric(impute.act$interval)
colSums(is.na(impute.act))

# c- Create a new dataset that is equal to the original dataset but with the missing data filled in.
summary(activity0)
summary(impute.act)

# d- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
# Summarize data by date
impute.act2 <- summarize(group_by(impute.act,date),daily.step=sum(steps))

mean.impute   <- as.integer(mean(impute.act2$daily.step))
sprintf("Mean number of steps/day = %g", mean.impute)

median.impute <- as.integer(median(impute.act2$daily.step))
sprintf("Median number of steps/day = %g", median.impute)

# 7. Histogram of the total number of steps taken each day after missing values are imputed

plot.steps.day <- ggplot(impute.act2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 2000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.impute, colour="red",  size=2) +
  geom_vline(xintercept=median.impute, colour="green" , linetype="dashed", size=2) +
  labs(title="Histogram of number of steps/day", y="Frequency", x="Steps/day") 
plot.steps.day





# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

# a- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a
#    given date is a weekday or weekend day.
impute.act$day <- ifelse(weekdays(as.Date(impute.act$date)) %in% c("Saturday","Sunday"),  "Weekend","Weekday")

# b- Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") 
#    of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or 
#    weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should 
#    look like using simulated data.

df <- impute.act %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))

plot.weekday.interval <- ggplot(df, aes(x=interval, y=mean.step, color=day)) + 
  facet_grid(day~.) +
  geom_line() + 
  labs(title="Average number of steps vs. time", y="Average number of steps", x="Time (x 5 minutes)")
plot.weekday.interval

# 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
