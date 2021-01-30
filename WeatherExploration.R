## WeatherExploration.R
library(dplyr)
library(ggplot2)
library(lubridate)


## Download the data file into the ./data directory

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipFile <- "data/StormData.csv.bz2"
download.file(url=url,destfile=zipFile,method="auto")

## Load the zipped CSV file into a data frame
zipFile <- "data/StormData.csv.bz2"
stormData1 <- read.csv(zipFile)

## For clarity, select only the columns relating to Date, location(state)
## Event Type, harm to persons, economic impact
StormFocus1 <- stormData1 %>% 
  select(BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP)

## For harm to persons we will sum the FATALITIES and INJURIES columns.
##
## For economic impact we will sum the PROPDMG and CROPDMG columns,
## after adjusting to use the appropriate scale factor or
## K -- Kilo, M -- Millions, or B -- Billions from the corresponding 
## PROPDMGEXP and CROPDMGEXP columns.
##
## For time of event we will just extract the year from BGN_DATE


## Define a function to multiple a damage cost column (PROPDMG or CROPDMG)
## by the appropriate scale factor.
mapCost <- function(cost, mult) {
  if(mult == "b" || mult == "B") {
    result <- cost * 1e9
  } else if(mult == "m" || mult == "M") {
    result <- cost * 1e6
  } else if(mult == "k" || mult == "K") {
    result <- cost * 1e3
  } else {
    result <- NA
  }
  result
}

year <- as.integer(sub(" 0:00:00","",sub("[0-9]+/[0-9]+/","",as.character(StormFocus1$BGN_DATE))))

StormFocus2 <- cbind(StormFocus1,year)

summary(StormFocus2)


StormFocus3 <- StormFocus2 %>% rowwise() %>%
  mutate(property_cost=mapCost(PROPDMG, PROPDMGEXP), 
         crop_cost=mapCost(CROPDMG, CROPDMGEXP)) %>%
           ungroup()

summary(StormFocus3)
         
         
  

StormFocus2 %>% group_by(CROPDMGEXP) %>% summarize(sum_crop=sum(CROPDMG), count= n())

StormFocus2 %>% group_by(PROPDMGEXP) %>% summarize(sum_prop=sum(PROPDMG), count = n())

str(StormFocus2)

xx <- sub("[0-9]+/[0-9]+/([0-9]+) ",as.character(StormFocus1$BGN_DATE),replacement="\1")

