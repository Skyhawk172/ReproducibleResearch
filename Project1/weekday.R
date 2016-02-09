# Clear environment variables:
rm(list = ls())

# Found online, in order to change the timezone 
# in scale_x_datetime:
date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}

library(dplyr)
library(ggplot2)
library(scales)
library(chron)

png("figures/weekday.png", width=680, height=400)

rawtable=read.csv("activity.csv")

missing <- filter(rawtable, is.na(steps)==TRUE)

# USE ROWS WITH VALID ENTRIES ONLY (i.e. NO NAs):
cleantable <- filter(rawtable, is.na(steps)==FALSE)

# Group by intervals
inter <- group_by(cleantable, interval)
meanSteps <- summarise(inter, meanSteps=median(steps))

# Now, fill in the values of the array "missing" with the median value:
for(i in 1:nrow(missing)){
  int <- missing$interval[i]
  missing$steps[i] <- meanSteps$meanSteps[meanSteps$interval==int]
}


# BIND BACK THE WHOLE ARRAY INCLUDING THE ORIGINAL ROWS WITH NAs:
allvalues <- rbind(cleantable, missing)
allvalues$date <- as.Date(allvalues$date, format="%Y-%m-%d" )

# CREATE FACTOR COLUMN FOR WEEK DAYS & WEEKEND DAYS:
dayofweek   <- is.weekend(allvalues$date) 
dayofweek.f <- factor(dayofweek, labels=c("Weekday","Weekend"))
finaltable  <- cbind(allvalues, dayofweek.f)

finaltable$interval <- strptime(sprintf("%04d", as.numeric(finaltable$interval)), format="%H%M")
finaltable$interval <- as.POSIXct(finaltable$interval, format="%H:%M:%S")

## GROUP BY INTERVALS AND WEEK DAY/WEEKEND:
inter <- group_by(finaltable, interval, dayofweek.f)
aveSteps <- summarise(inter, meanSteps=mean(steps))

# Plot results and add linear fit:
g <- ggplot(aveSteps, aes(interval, meanSteps)) + geom_point() + geom_line()
g <- g + labs(title = "Average # steps per day interval") 
g <- g + labs(x = "Day Interval", y = "Average # steps")
g <- g + facet_wrap(~dayofweek.f, ncol=1)#facet_grid(dayofweek.f ~ .) # + ylim(0,120)
g <- g + scale_x_datetime(date_breaks="2 hour", labels=date_format_tz("%H:%M", tz="EST"))

print(g)
dev.off()

