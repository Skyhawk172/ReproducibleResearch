library(dplyr)
library(ggplot2)

downloaded_csv <-"repdata-data-StormData.csv"

# LOADS DATA SET ONLY ONCE:
if (!exists("originaldata")) {
  print("Reading data file...")
  originaldata   <- read.csv(downloaded_csv)
}
stormdata <- originaldata

# ADD COLUMN FOR MY OWN SUMMARY CLASSIFICATION:
stormdata <- mutate(stormdata, GEN_EVTYPE=EVTYPE)
stormdata$GEN_EVTYPE <- as.character(stormdata$GEN_EVTYPE)

stormdata <- mutate(stormdata, CASUALTIES=FATALITIES+INJURIES)


# USE EXPONENT TO DETERMINE SCALE FACTOR FOR DAMAGES:
stormdata <- mutate(stormdata, PROPDMGEXP_INT = ifelse(PROPDMGEXP == "B", 1000000000.,
                                     ifelse(PROPDMGEXP == "M", 1000000.,
                                     ifelse(PROPDMGEXP == "K", 1000.,
                                     ifelse(PROPDMGEXP == "H", 100., 0.)))) )

stormdata <- mutate(stormdata, CROPDMGEXP_INT = ifelse(CROPDMGEXP == "B", 1000000000.,
                                     ifelse(CROPDMGEXP == "M", 1000000.,
                                     ifelse(CROPDMGEXP == "K", 1000.,
                                     ifelse(CROPDMGEXP == "H", 100., 0.)))) )

stormdata <- mutate(stormdata, TOTALLOSS = PROPDMG*PROPDMGEXP_INT + CROPDMG*CROPDMGEXP_INT)

# DOWN SELECT ONLY RELEVANT COLUMNS:
stormdata <- select(stormdata, STATE, EVTYPE, GEN_EVTYPE, CASUALTIES, TOTALLOSS)


# NEED TO CLEAN THE DATA EVENT TYPE. SOME ARE ALL CAPS, ALL 
# SMALL LETTERS, DIFFERENT WORDING, ETC.
# START GROUPING SOME EVTYPE TOGETHER AS MANY ARE SYNONYM:

stormdata$EVTYPE <- toupper(stormdata$EVTYPE)
keys=c("SNOW", "RAIN", "HAIL", "WIND", "TORNADO", "^LIGHTNING", "FLOOD", "HURRICANE", "AVAL", 
       "FREEZE", "BURST", "DUST", "SURF", "TIDE", "COLD", "HEAT", "FIRE", "^TROPICAL STORM", 
       "BLIZZARD", "FOG", "RIP CURRENT", "SUMMARY")

for(ievent in keys){
  idx  <- grep(paste("*",ievent,"*",sep=""), stormdata$EVTYPE)
  stormdata$GEN_EVTYPE[idx] = paste(ievent,"RELATED")
}

# SPECIAL CASE: THUNDERSTORM WINDS:
idx  <- grep("*TSTM*", stormdata$EVTYPE)
idx  <- c(idx, grep("*THUNDERSTORM*", stormdata$EVTYPE))
stormdata$GEN_EVTYPE[idx] = "THUNDERSTORM RELATED"


# FILTER THE REPORTED EVENTS THAT RESULTED IN FATALITIES/INJURIES
# OR DAMAGES AND GROUP THEM BY EVENT TYPE:
casualties <- group_by( filter(stormdata, CASUALTIES>0), GEN_EVTYPE)
sum_casualties <- summarise(casualties, sum=sum(CASUALTIES))
sum_casualties <- arrange(sum_casualties, desc(sum))
sum_casualties <- sum_casualties[1:15,]
sum_casualties$sum <- sum_casualties$sum / 1e3

damages <- group_by( filter(stormdata, TOTALLOSS>0), GEN_EVTYPE)
sum_damages <- summarise(damages, sum=sum(TOTALLOSS))
sum_damages <- arrange(sum_damages, desc(sum))
sum_damages <- sum_damages[1:15,] 
sum_damages$sum <- sum_damages$sum / 1e9


# PLOT RESULTS:
png("damages.png", width=680, height=400)
g <- ggplot(sum_damages, aes(reorder(GEN_EVTYPE,sum), sum)) 
g <- g + geom_bar(fill="red", col="black", stat="identity") # stat="identity" b/c plotting the column value.
g <- g + xlab("Event Type") + ylab("Total Damages (Billions $)") 
g <- g + coord_flip()  # Showing the events name on the Y axis since it's easier to read.
g <- g + labs(title="Property/Crop Damage by event type") + theme_bw()
print(g)
dev.off()

png("casualties.png", width=680, height=400)
g <- ggplot(sum_casualties, aes(reorder(GEN_EVTYPE,sum), sum)) 
g <- g + geom_bar(fill="red", col="black", stat="identity") # stat="identity" b/c plotting the column value.
g <- g + xlab("Event Type") + ylab("Total Casualities (x1000)") 
g <- g + coord_flip()  # Showing the events name on the Y axis since it's easier to read.
g <- g + labs(title="Casualities by event type") + theme_bw()
print(g)
dev.off()