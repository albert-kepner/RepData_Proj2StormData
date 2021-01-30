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
  select(BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
