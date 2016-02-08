# Clear environment variables:
rm(list = ls())

library(dplyr)
library(ggplot2)

png("histo.png", width=480, height=480)

mytable=read.csv("activity.csv")

# IGNORE NAs FOR NOW:
mytable <- filter(mytable, is.na(steps)==FALSE)
mytable$date <- as.Date(mytable$date, format="%Y-%m-%d" )

daily <- group_by(mytable, date)
totalSteps <- summarise(daily, sumSteps=sum(steps))

meanSteps=mean(  totalSteps$sumSteps)
mediSteps=median(totalSteps$sumSteps)

nbins=max(totalSteps$sumSteps)/1000

hist( totalSteps$sumSteps, breaks=nbins, 
      col="red",
      xlab='Daily # of steps', 
      ylim=c(0,20),
      main="Total Daily Number of Steps")
legend(500, 10, legend = c(paste("Mean =", round(meanSteps, 1)),
                            paste("Median =",round(mediSteps, 1))), bty = "n")

dev.off()