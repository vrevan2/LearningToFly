month = 1
Airport1 = 'ORD'
Airport2 = 'MDW'

dataset<-read.csv("On_Time_Performance_2017_IL.csv")

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