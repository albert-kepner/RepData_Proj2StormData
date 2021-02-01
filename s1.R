t1 <- StormFocus3 %>%
  filter(EVTYPE == "TORNADO")

t2 <- t1 %>% group_by(year)

t3 <- t2 %>% summarise(Fatalities=sum(FATALITIES),Injuries=sum(INJURIES))

TornadoHarmToPersonsByYear <- t3

