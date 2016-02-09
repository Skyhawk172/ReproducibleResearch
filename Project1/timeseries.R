# Clear environment variables:
rm(list = ls())

date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}

library(dplyr)
library(ggplot2)
library(scales)

png("figures/timeseries.png", width=880, height=880)

mytable=read.csv("activity.csv")

# IGNORE NAs FOR NOW:
mytable <- filter(mytable, is.na(steps)==FALSE)
mytable$date <- as.Date(mytable$date, format="%Y-%m-%d" )

mytable$interval <- strptime(sprintf("%04d", as.numeric(mytable$interval)), format="%H%M")
mytable$interval <- as.POSIXct(mytable$interval, format="%H:%M:%S")

#print(mytable$interval)

# group by intervals
inter <- group_by(mytable, interval)
meanSteps <- summarise(inter, meanSteps=mean(steps))

timemax <- meanSteps$interval[which.max(meanSteps$meanSteps)]
print(timemax)

g <- ggplot(meanSteps, aes(interval, meanSteps)) + geom_point() + geom_line()
g <- g + labs(title = "Average # steps as a function of time in day") 
g <- g + labs(x = "Time", y = "Average # of steps")
g <- g + scale_x_datetime(date_breaks="2 hour", labels=date_format_tz("%H:%M", tz="EST"))

print(g)
dev.off()
