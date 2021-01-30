## WeatherExploration.R
library(dplyr)
library(ggplot2)
library(lubridate)


## Download the data file into the ./data directory

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipFile <- "data/StormData.csv.bz2"
download.file(url=url,destfile=zipFile,method="auto")

stormData1 <- read.csv(zipFile)
dataSize <- object.size(stormData1)
dataSize

str(stormData1)
summary(stormData1)
names(stormData1)


## For clarity, select only the columns relating to Date, location(state)
## Event Type, harm to persons, economic impact

StormFocus1 <- stormData1 %>% 
  select(BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP)

## For harm to persons we will sum the FATALITIES and INJURIES columns.
## For economic impact we will sum the PROPDMGEXP and CROPDMGEXP columns.
## For time of event we will just extract the year from BGN_DATE

dates1 <- head(StormFocus1$BGN_DATE)

dates2 <- mdy_hms(dates1)

dates3 <- as.Date(dates2)

format(as.Date(dates2, format="%d/%m/%Y"),"%Y")

years <- as.numeric(format(as.Date(StormFocus1$BGN_DATE, format="%d/%m/%Y"),"%Y"))

summary(years)


## StormFocus2 <- StormFocus1 %>% 
##  mutate(year=as.numeric(format(as.Date(StormFocus1$BGN_DATE, format="%d/%m/%Y"),"%Y"))) %>%
##  mutate(harm_to_persons = FATALITIES + INJURIES) %>%
##  mutate(economic_harm = PROPDMG + CROPDMG)

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

StormFocus2 <- StormFocus1 %>% 
  mutate(year=as.numeric(format(as.Date(StormFocus1$BGN_DATE, format="%d/%m/%Y"),"%Y")))

StormFocus3 <- StormFocus2 %>% rowwise() %>%
  mutate(prop_cost=mapCost(PROPDMG, PROPDMGEXP), 
         crop_cost=mapCost(CROPDMG, CROPDMGEXP)) %>%
           ungroup()
         
         
  

StormFocus2 %>% group_by(CROPDMGEXP) %>% summarize(sum_crop=sum(CROPDMG), count= n())

StormFocus2 %>% group_by(PROPDMGEXP) %>% summarize(sum_prop=sum(PROPDMG), count = n())

str(StormFocus2)