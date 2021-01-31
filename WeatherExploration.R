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


## Define a function to multiply a damage cost column (PROPDMG or CROPDMG)
## by the appropriate scale factor.
mapCost <- function(cost, mult) {
  if(mult == "b" || mult == "B") {
    result <- cost * 1e9
  } else if(mult == "m" || mult == "M") {
    result <- cost * 1e6
  } else if(mult == "k" || mult == "K") {
    result <- cost * 1e3
  } else {
    result <- cost
  }
  result
}
### This transforms the BGN_DATE in format "m/d/y hh:mm:ss" to just the year as an integer.
year <- as.integer(sub(" 0:00:00","",sub("[0-9]+/[0-9]+/","",as.character(StormFocus1$BGN_DATE))))

StormFocus2 <- cbind(StormFocus1,year)

summary(StormFocus2)


StormFocus3 <- StormFocus2 %>% rowwise() %>%
  mutate(property_cost=mapCost(PROPDMG, PROPDMGEXP), 
         crop_cost=mapCost(CROPDMG, CROPDMGEXP),
         total_cost=sum(property_cost, crop_cost, na.rm=TRUE),
         harm_to_persons=sum(FATALITIES,INJURIES, na.rm=TRUE)) %>%
           ungroup()

StormFocus4 <- StormFocus3 %>% rename(Event_Type = EVTYPE) %>%  
  select(STATE, year, Event_Type, FATALITIES, INJURIES, harm_to_persons, 
         property_cost, crop_cost, total_cost)
         

summary(StormFocus4)

## Rank Event_Type by total_cost descending (summed across all years and locations)

TotalCostRank <- StormFocus4 %>% group_by(Event_Type) %>%
  summarize(TotalCost = sum(total_cost), Count=n()) %>%
  arrange(desc(TotalCost))

TotalCostRank

## Rank Event_type by harm_to_persons descending(summed across all years and locations)

HarmToPersonsRank <- StormFocus4 %>% group_by(Event_Type) %>%
  summarize(HarmToPersons = sum(harm_to_persons), Count=n()) %>%
  arrange(desc(HarmToPersons))

HarmToPersonsRank


## We have found that TORNADO events been the greatest contributor to
## Harm to persons in aggregate over the the entire period.
## We would now like to construct a plot which shows how TORNADO related
## FATALITIES and INJURIES have varied by year.

summary(StormFocus3)
## StormFocus3 has selected columns of all observations from the original data set,
## and includes the columns we want, YEAR, FATALITIES, INJURIES. We
## want to group and summarize this data by year.
HarmToPersonsByYEAR <- StormFocus3 %>%
  filter(EVTYPE == "TORNADO") %>%
  group_by(YEAR) %>%
  summarize(Fatalities=sum(FATALITIES),Injuries=sum(INJURIES))

