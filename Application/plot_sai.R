

#### Globals
defaultTz <- "America/Chicago"

## Raw Data
df <- read.csv("data/OTP_2017.csv")

## Airport Info
airports <- read.csv("data/airports_stations.csv")
rownames(airports) <- as.character(airports$IATA)

#Airlines Lookup
airlines <- read.csv("data/airlines.csv")

month = 1
Airport1 = 'ORD'
Airport2 = 'MDW'
dfMonth<-dataset[dataset$ArrMonthofYear == month | dataset$DepMonthofYear == month,]


### number of arrivals and departures for day of the week across a month
dfMonthDoW<- dfMonth[dfMonth$Dest == Airport1 | dfMonth$Origin == Airport1| dfMonth$Dest == Airport2 | dfMonth$Origin == Airport2,]
dfMonthDoW <- dfMonthDoW[,c('DayOfWeek', 'Dest', 'Origin')]
colnames(dfMonthDoW)<-c('Day', 'Arr', 'Dep')
dfMonthDoW$Arr<-sapply(dfMonthDoW$Arr, function(x) if(is.na(x)) x=0 else if(x == Airport1) x=1 else if (x==Airport2) x=2 else x=0)
dfMonthDoW$Dep<-sapply(dfMonthDoW$Dep, function(x) if(is.na(x)) x=0 else if(x == Airport1) x=1 else if (x==Airport2) x=2 else x=0)
dfMonthDoWMelt <- melt(dfMonthDoW, id='Day')
dfMonthDoWMelt<-dfMonthDoWMelt[!((dfMonthDoWMelt$variable == 'Arr' & dfMonthDoWMelt$value==0) | (dfMonthDoWMelt$variable == 'Dep' & dfMonthDoWMelt$value==0)),]
dfMonthDoWMelt$cat<-''
dfMonthDoWMelt[dfMonthDoWMelt$value == 1,]$cat <- Airport1
dfMonthDoWMelt[dfMonthDoWMelt$value == 2,]$cat <- Airport2

ggplot(dfMonthDoWMelt, aes(x = cat, fill = variable)) + geom_bar(stat = 'count', position = 'stack') + facet_grid(~ Day) + scale_fill_manual(labels=c('Arrivals', 'Departures'), values = c('#7a8da8', '#c6adaa'))

### number of arrivals and departures for hour of the day across a month

dfMonthHoD<- dfMonth[dfMonth$Dest == Airport1 | dfMonth$Origin == Airport1| dfMonth$Dest == Airport2 | dfMonth$Origin == Airport2,]
dfMonthHoD<-dfMonthHoD[,c('ArrHourofDay', 'DepHourofDay', 'Dest', 'Origin')]
dfMonthHoD$airport <- ''
dfMonthHoD<- na.omit(dfMonthHoD)
dfMonthHoD[dfMonthHoD$Origin == Airport1 | dfMonthHoD$Dest == Airport1,]$airport <- 1
dfMonthHoD[dfMonthHoD$Origin == Airport2 | dfMonthHoD$Dest == Airport2,]$airport <- 2
dfMonthHoD<-data.frame('Arrivals' = dfMonthHoD$ArrHourofDay, 'Departures' = dfMonthHoD$DepHourofDay, 'airport' = dfMonthHoD$airport)
dfMonthHoDMelt<-melt(dfMonthHoD, id='airport')
dfMonthHoDMelt$airport <- ifelse(dfMonthHoDMelt$airport == 1, Airport1, Airport2)

ggplot(dfMonthHoDMelt, aes(x = airport, fill = variable)) + geom_bar(stat = 'count', position = 'stack') + facet_grid(~ value) + scale_fill_manual(labels=c('Arrivals', 'Departures'), values = c('#7a8da8', '#c6adaa'))


### number of delays for hour of the day across a month

dfdelay<-dfMonth[(dfMonth$Dest == Airport1 | dfMonth$Dest == Airport2) & dfMonth$ArrDelay > 0,]
dfdelay<-dfdelay[,c('Dest','ArrDelay', 'CRSArrHourofDay')]

ggplot(dfdelay, aes(x = Dest, fill = Dest)) + geom_bar(stat = 'count') + facet_grid(~ CRSArrHourofDay) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


DistDepA1 <- df %>% filter(Origin == Airport1) %>% dplyr::count(DistanceGroup)
DistArrA1 <- df %>% filter(Dest == Airport1) %>% dplyr::count(DistanceGroup)
DistA1 <- merge(DistArrA1, DistDepA1, by='DistanceGroup', all = TRUE, suffixes=c('Arr', 'Dep'))
plot_ly(DistA1, x = ~DistanceGroup, y = ~nArr, type = 'bar', name = 'Arrivals') %>% add_trace(y = ~nDep, name = 'Departures') %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

etDepA1 <- df %>% filter(Origin == Airport1) %>% dplyr::count(CRSElapsedTimeGroup)
etArrA1 <- df %>% filter(Dest == Airport1) %>% dplyr::count(CRSElapsedTimeGroup)
ETimeA1 <- merge(etArrA1, etDepA1, by='CRSElapsedTimeGroup', all = TRUE, suffixes=c('Arr', 'Dep'))
ETimeA1[is.na(ETimeA1)] <- 0
plot_ly(ETimeA1, x = ~CRSElapsedTimeGroup, y = ~nArr, type = 'scatter', mode = 'lines', name = 'Arrivals') %>% add_trace(y = ~nDep, name = 'Departures') %>% layout(yaxis = list(title = 'Count'))

Airport2Dep <- dfMonth %>% filter(Origin == Airport2) %>% count('hour' = DepHour)
Airport1Dep <- dfMonth %>% filter(Origin == Airport1) %>% count('hour' = DepHour)
Airport1Arr <- dfMonth %>% filter(Dest == Airport1) %>% count('hour' = ArrHour)
Airport2Arr <- dfMonth %>% filter(Dest == Airport2) %>% count('hour' = ArrHour)
Airport1ArrDep<-merge(Airport1Arr, Airport1Dep, by = 'hour', all = TRUE, suffixes = c('Arr', 'Dep'))
Airport2ArrDep<-merge(Airport2Arr, Airport2Dep, by = 'hour', all = TRUE, suffixes = c('Arr', 'Dep'))
tableArrDepByHour<-merge(Airport1ArrDep, Airport2ArrDep, by = 'hour', all = TRUE)