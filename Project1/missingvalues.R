# Clear environment variables:
rm(list = ls())

library(dplyr)
library(ggplot2)
library(scales)

png("missingvalues.png", width=480, height=480)

mytable=read.csv("activity.csv")

missing <- filter(mytable, is.na(steps)==TRUE)
missing$date <- as.Date(missing$date, format="%Y-%m-%d" )
print(nrow(missing))

# IGNORE NAs FOR NOW:
cleantable <- filter(mytable, is.na(steps)==FALSE)
cleantable$date <- as.Date(cleantable$date, format="%Y-%m-%d" )

# group by intervals
inter <- group_by(cleantable, interval)
meanSteps <- summarise(inter, meanSteps=median(steps))

# Now, fill in the values of array "missing":
for(i in 1:nrow(missing)){
  int <- missing$interval[i]
  missing$steps[i] <- meanSteps$meanSteps[meanSteps$interval==int]
  #cat(sprintf("\"%f\" \"%f\"\n", i, int))
}


allvalues <- rbind(cleantable, missing)
allvalues$date <- as.Date(mytable$date, format="%Y-%m-%d" )

# group by intervals
date_groups <- group_by(allvalues, date)
all <- summarise(date_groups, sumSteps=sum(steps))

meanSteps=mean(  all$sumSteps)
mediSteps=median(all$sumSteps)

print(meanSteps)
print(mediSteps)
nbins=max(all$sumSteps)/1000

hist( all$sumSteps, breaks=nbins, 
      col="red",
      xlab='Daily # of steps', 
      ylim=c(0,20),
      main="Total Daily Number of Steps")
legend(500, 10, legend = c(paste("Mean =", round(meanSteps, 1)),
                          paste("Median =",round(mediSteps, 1))), bty = "n")


dev.off()
