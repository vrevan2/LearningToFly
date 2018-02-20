
#### Environment Setup ( Change This )####

setwd("C:/Users/Prakash/Desktop/CS 424") 

####

#### Global Variables Creation Here ####


dataset<-read.csv("On_Time_Performance_2017_IL.csv")






# changing some dates that were of the form dd-mm-yyy to dd/mm/yyyy

dataset$FlightDate <- gsub("-", "/" ,dataset$FlightDate )

# Scheduled Departure Times

# Some Scheduled Departure Times weren't starting with 0 e.g 328 should be 0328

dataset$CRSDepTime[nchar(dataset$CRSDepTime)==3]<-paste0( "0",dataset$CRSDepTime[nchar(dataset$CRSDepTime)==3]) 
dataset$CRSDepTime[nchar(dataset$CRSDepTime)==2]<-paste0( "00",dataset$CRSDepTime[nchar(dataset$CRSDepTime)==2]) 
dataset$CRSDepTime[nchar(dataset$CRSDepTime)==1]<-paste0( "000",dataset$CRSDepTime[nchar(dataset$CRSDepTime)==1]) 

# checking for both mm/dd and dd/mm formats
dataset$temp<-paste(dataset$FlightDate , dataset$CRSDepTime)
dataset$OriginDateTime<-strptime(dataset$temp, format="%d/%m/%Y %H%M")
dataset$OriginDateTime[is.na(dataset$OriginDateTime)]<-strptime(dataset$temp[is.na(dataset$OriginDateTime)], format="%m/%d/%Y %H%M")

# Scheduled Arrival Times

dataset$CRSArrTime[nchar(dataset$CRSArrTime)==3]<-paste0( "0",dataset$CRSArrTime[nchar(dataset$CRSArrTime)==3]) 
dataset$CRSArrTime[nchar(dataset$CRSArrTime)==2]<-paste0( "00",dataset$CRSArrTime[nchar(dataset$CRSArrTime)==2]) 
dataset$CRSArrTime[nchar(dataset$CRSArrTime)==1]<-paste0( "000",dataset$CRSArrTime[nchar(dataset$CRSArrTime)==1]) 

dataset$temp<-paste(dataset$FlightDate , dataset$CRSArrTime)
dataset$DestDateTime<-strptime(dataset$temp, format="%d/%m/%Y %H%M")
dataset$DestDateTime[is.na(dataset$DestDateTime)]<-strptime(dataset$temp[is.na(dataset$DestDateTime)], format="%m/%d/%Y %H%M")

# Arrival Times
dataset$ArrTime[nchar(dataset$ArrTime)==3 & !is.na(dataset$ArrTime)]<-paste0( "0",dataset$ArrTime[nchar(dataset$ArrTime)==3 & !is.na(dataset$ArrTime)]) 
dataset$ArrTime[nchar(dataset$ArrTime)==2 & !is.na(dataset$ArrTime)]<-paste0( "00",dataset$ArrTime[nchar(dataset$ArrTime)==2 & !is.na(dataset$ArrTime)]) 
dataset$ArrTime[nchar(dataset$ArrTime)==1 & !is.na(dataset$ArrTime)]<-paste0( "000",dataset$ArrTime[nchar(dataset$ArrTime)==1 & !is.na(dataset$ArrTime)]) 

dataset$temp<-paste(dataset$FlightDate , dataset$ArrTime)
dataset$ArrDateTime<-strptime(dataset$temp, format="%d/%m/%Y %H%M")
dataset$ArrDateTime[is.na(dataset$ArrDateTime)]<-strptime(dataset$temp[is.na(dataset$ArrDateTime)], format="%m/%d/%Y %H%M")

# Departure Times

dataset$DepTime[nchar(dataset$DepTime)==3 & !is.na(dataset$DepTime)]<-paste0( "0",dataset$DepTime[nchar(dataset$DepTime)==3 & !is.na(dataset$DepTime)]) 
dataset$DepTime[nchar(dataset$DepTime)==2 & !is.na(dataset$DepTime)]<-paste0( "00",dataset$DepTime[nchar(dataset$DepTime)==2 & !is.na(dataset$DepTime)]) 
dataset$DepTime[nchar(dataset$DepTime)==1 & !is.na(dataset$DepTime)]<-paste0( "000",dataset$DepTime[nchar(dataset$DepTime)==1 & !is.na(dataset$DepTime)]) 

dataset$temp<-paste(dataset$FlightDate , dataset$DepTime)
dataset$DepDateTime<-strptime(dataset$temp, format="%d/%m/%Y %H%M")
dataset$DepDateTime[is.na(dataset$DepDateTime)]<-strptime(dataset$temp[is.na(dataset$DepDateTime)], format="%m/%d/%Y %H%M")




#### End of Global Variables Creation Section ####


#### New Variables/Columns Declared : (Add declared Variables here)

# DepDateTime, ArrDateTime -  Have NAs as the flights might be cancelled
# OriginDateTime, DestDateTime - Scheduled Times, Always present

####



#### Plots Names Declared #####
 

####

#### Plots ####


#### End of Plots ####