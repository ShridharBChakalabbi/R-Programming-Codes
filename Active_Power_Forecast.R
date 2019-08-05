################Import Packages##################
library(dplyr)
library(lubridate)
library(chron)
library(plyr)
library("ggpubr")
library("doBy")
library(caret)
library(sqldf)
library(TSA)
library(tseries)
library(forecast)
library(nnet)
library(tibble)
library(zoo)
library(imputeTS)
library(randomForest)
library(stargazer)
library(data.table)
library(rio)
library("aws.s3")

###############Obtain Current Date and Time########
Run_Time <- hour(Sys.time())
Prediction_Date <- as.Date(Sys.time(), format = "%Y-%m-%d") + 1
Prediction_Time <- as.character(Sys.time())
Prediction_Time <- substr(Prediction_Time, 12, 16)

###########################Data Preparation#################
##################Meter Data Read####################
New_Mtr_raw <- read.csv("/home/sunil/single_code/Raw_Data/Barod meter_raw.csv")
Mtr_raw <- New_Mtr_raw
Mtr_raw <- Mtr_raw[,1:10]
str(Mtr_raw)
sum(is.na(Mtr_raw))
Mtr_raw[Mtr_raw == "" | Mtr_raw == "*"] <- NA
Mtr_raw$Date_DD_MM_YYYY <- as.Date(Mtr_raw$Date_DD_MM_YYYY, format = "%Y-%m-%d")
Mtr_raw$Time_Stamp <- times(strftime(as.POSIXct(as.character(Mtr_raw$Time_Stamp), format = "%H:%M"), "%H:%M:%S"))
Mtr_raw[3:10] <- as.numeric(as.matrix(Mtr_raw[3:10]))
#####################Rbind Prev Day File###################
prev_date <- as.Date(Sys.time(), format = "%Y-%m-%d") - 1
prev_date <- gsub("-","_", prev_date)
setwd("/home/sunil/single_code/Raw_Data/Excel_Files/")
file_to_check <- paste('Todays Clean Energy,Barod_MeterData_', prev_date,'.xls', sep = "")
if (file.exists(file_to_check) == TRUE) {
  if(Run_Time == 11) {
    convert(paste('/home/sunil/single_code/Raw_Data/Excel_Files/Todays Clean Energy,Barod_MeterData_', prev_date,'.xls', sep = ""), paste('/home/sunil/single_code/Raw_Data/Excel_Files/Todays Clean Energy,Barod_MeterData_', prev_date,'.csv', sep = ""))
    i <- read.csv(paste("/home/sunil/single_code/Raw_Data/Excel_Files/Todays Clean Energy,Barod_MeterData_",prev_date,".csv", sep = ""),skip = 2,stringsAsFactors = T)
    names(i) <- gsub("\\.+","_", names(i))
    i[i == ""| i == "*"] <- NA
    #i <- i[-which(i$Time_Stamp ==""),]
    i <- i[3:nrow(i),1:9]
    row.names(i) <- NULL
    Previous_Date <- data.frame(Date_DD_MM_YYYY = as.Date(Sys.time(), format = "%Y-%m-%d") - 1)
    i <- cbind(Previous_Date,i)
    i$Time_Stamp <- times(as.numeric(as.character(i$Time_Stamp)))
    i[3:10] <- as.numeric(as.matrix(i[3:10]))
    colnames(i)[9] <- "Energy_Export_KWHE_1"
    colnames(i)[10] <- "Energy_Export_KWHE_2"
    Mtr_raw <- rbind(Mtr_raw,i)
    write.csv(Mtr_raw,'/home/sunil/single_code/Raw_Data/Barod meter_raw.csv', row.names = F)
  }
}
#########################Function- fill_NA_Gaps###############
fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  
  x
}
##############################################################
Mtr_raw <- Mtr_raw[,c(1,2,3,4)]
colnames(Mtr_raw)[3] <- "Active_Power_KW_1"
colnames(Mtr_raw)[4] <- "Active_Power_KW_2"
#Mtr_raw <- mutate(Mtr_raw,Energy_Export_KWHE_1 = Energy_Export_KWHE_1 - lag(Energy_Export_KWHE_1))
#Mtr_raw <- mutate(Mtr_raw,Energy_Export_KWHE_2 = Energy_Export_KWHE_2 - lag(Energy_Export_KWHE_2))
#a <- subset(Mtr_raw, chron::hours(Mtr_raw$Time_Stamp) < 5)
#a1 <- subset(Mtr_raw, chron::hours(Mtr_raw$Time_Stamp) > 19)
#a$Active_Power_KW_1 <- 0
#a$Active_Power_KW_2 <- 0
#a1$Active_Power_KW_1 <- 0
#a1$Active_Power_KW_2 <- 0
#a2 <- subset(Mtr_raw, chron::hours(Mtr_raw$Time_Stamp) >= 5 & chron::hours(Mtr_raw$Time_Stamp) <=19)
a2 <- Mtr_raw
##################Meter Data Preparation####################
c <- c()  #initialize null vector
for (i in unique(a2$Date_DD_MM_YYYY)) {
  q <- a2[a2$Date_DD_MM_YYYY == i,]
  if(sum(is.na(q$Active_Power_KW_1)) > 0.1*nrow(q) | nrow(q) < 40) {
    next
  }
  b <- data.frame(NA_Rows = rownames(q))
  c <- rbind(c,b)
}
a2 <- a2[as.numeric(rownames(a2)) %in% c$NA_Rows, ]
Mtr_raw <- a2
Mtr_raw <- orderBy(~Date_DD_MM_YYYY+Time_Stamp,data = Mtr_raw)
row.names(Mtr_raw) <- NULL
#Mtr_raw$Energy_Export_KWHE_ <- Mtr_raw$Energy_Export_KWHE_1+Mtr_raw$Energy_Export_KWHE_2
#Mtr_raw[,c(3,4)] <- NULL
#Mtr_raw$Energy_Export_KWHE_ <- ifelse(Mtr_raw$Energy_Export_KWHE_<=0, 0, Mtr_raw$Energy_Export_KWHE_)
#Mtr_raw$Energy_Export_KWHE_ <- ifelse(Mtr_raw$Energy_Export_KWHE_ > 6000, 0 , Mtr_raw$Energy_Export_KWHE_)
Mtr_raw$Active_Power_KW_1 <- na.interpolation(Mtr_raw$Active_Power_KW_1)
Mtr_raw$Active_Power_KW_2 <- na.interpolation(Mtr_raw$Active_Power_KW_2)
##################Mid Zero Treatment####################
# a <- subset(Mtr_raw, chron::hours(Mtr_raw$Time_Stamp) >= 7 & chron::hours(Mtr_raw$Time_Stamp) <18)
# a$Energy_Export_KWHE_ <- ifelse(a$Energy_Export_KWHE_ == 0, NA, a$Energy_Export_KWHE_)
# t <- subset(Mtr_raw, chron::hours(Mtr_raw$Time_Stamp)<7)
# t1 <- subset(Mtr_raw,chron::hours(Mtr_raw$Time_Stamp) >= 18)
# Mtr_raw <- rbind(a,t,t1)
# Mtr_raw <- orderBy(~Date_DD_MM_YYYY+Time_Stamp,data = Mtr_raw)
# Mtr_raw$Energy_Export_KWHE_ <- na.interpolation(Mtr_raw$Energy_Export_KWHE_)

#############NA Dates removal##################################
# c <- c()
# for (j in unique(Mtr_raw$Date_DD_MM_YYYY)) {
#   z <- Mtr_raw[Mtr_raw$Date_DD_MM_YYYY == j,]
#   if((5 %in% chron::hours(z$Time_Stamp) & 18 %in% chron::hours(z$Time_Stamp)) == FALSE) {
#     next
#   }
#   c <- rbind(c,z)
# }
# Mtr_raw <- c
# row.names(Mtr_raw) <- NULL
Mtr_raw1 <- Mtr_raw[1:2443,]
Mtr_raw2 <- Mtr_raw[2444:nrow(Mtr_raw),]
##########################15mins conversion####################
Exist_Dates <- data.frame(Date_DD_MM_YYYY=unique(Mtr_raw1$Date_DD_MM_YYYY))
Time_Seq <- data.frame(Time_Stamp = seq(ymd_hm('2015-01-01 00:00'),ymd_hm('2016-12-31 23:45'), by = '5 mins')[1:288])
Time_Seq$Time_Stamp <- times(strftime(as.character(Time_Seq$Time_Stamp), "%H:%M:%S"))
asdf <- merge(Exist_Dates, Time_Seq, all.x = T)
asdf <- orderBy(~Date_DD_MM_YYYY+Time_Stamp,data = asdf)
row.names(asdf) <- NULL
qwer <- merge(asdf,Mtr_raw1,all.x = T)
qwer$Active_Power_KW_1 <- qwer$Active_Power_KW_1/2
qwer$Active_Power_KW_2 <- qwer$Active_Power_KW_2/2
qwer$Active_Power_KW_1 <- na.locf(qwer$Active_Power_KW_1)
qwer$Active_Power_KW_2 <- na.locf(qwer$Active_Power_KW_2)
qwer$num <- gl(nrow(asdf)/3,3)
orig <- sqldf("select Date_DD_MM_YYYY,num, sum(Active_Power_KW_1), sum(Active_Power_KW_2) from qwer group by num")
orig <- orderBy(~Date_DD_MM_YYYY+num,orig)
row.names(orig) <- NULL
Time_Seq1 <- data.frame(Time_Stamp = seq(ymd_hm('2015-01-01 00:00'),ymd_hm('2016-12-31 23:45'), by = '15 mins')[1:96])
Time_Seq1$Time_Stamp <- times(strftime(as.character(Time_Seq1$Time_Stamp), "%H:%M:%S"))
orig_Time <- merge(Exist_Dates,Time_Seq1,all.x = T)
orig_Time <- orderBy(~Date_DD_MM_YYYY+Time_Stamp,orig_Time)
row.names(orig_Time) <- NULL
orig_Time$Active_Power_KW_1 <- orig$`sum(Active_Power_KW_1)`
orig_Time$Active_Power_KW_2 <- orig$`sum(Active_Power_KW_2)`
Mtr_raw1 <- orig_Time

Exist_Dates2 <- data.frame(Date_DD_MM_YYYY=unique(Mtr_raw2$Date_DD_MM_YYYY))
#Time_Seq2 <- data.frame(Time_Stamp = seq(ymd_hm('2015-01-01 00:00'),ymd_hm('2016-12-31 23:45'), by = '5 mins')[1:96])
#Time_Seq2$Time_Stamp <- times(strftime(as.character(Time_Seq$Time_Stamp), "%H:%M:%S"))
asdf <- merge(Exist_Dates2, Time_Seq1, all.x = T)
asdf <- orderBy(~Date_DD_MM_YYYY+Time_Stamp,data = asdf)
row.names(asdf) <- NULL
qwer <- merge(asdf,Mtr_raw2,all.x = T)
qwer$Active_Power_KW_1 <- na.interpolation(qwer$Active_Power_KW_1)
qwer$Active_Power_KW_2 <- na.interpolation(qwer$Active_Power_KW_2)
Mtr_raw2 <- qwer

Mtr_raw <- rbind(Mtr_raw1,Mtr_raw2)

rm(b);rm(c);rm(a2);rm(q);rm(asdf);rm(qwer);rm(orig_Time);rm(orig);rm(Mtr_raw1);rm(Mtr_raw2)
#######Weather Data from Python & Meteoblue#####################
WMS_raw_worldweather <- read.csv("/home/sunil/single_code/Raw_Data/WMS_raw_worldweather.csv")
WMS_raw_GFS <- read.csv("/home/sunil/single_code/Raw_Data/WMS_raw_GFS.csv")
str(WMS_raw_worldweather)
summary(WMS_raw_worldweather)
WMS_raw_worldweather$Date_DD_MM_YYYY <- as.Date(WMS_raw_worldweather$Date_DD_MM_YYYY, format  = "%Y-%m-%d")
###############Rbind Previous Date Weather Data#################
prev_date <- as.Date(Sys.time(), format = "%Y-%m-%d") - 1
df <- file.info(list.files("/home/sunil/updates/", full.names = T))
prev_day_WMS_Data_files <- df[which(substr(rownames(df),22,40)== paste("WMS_Data_",prev_date,sep = "")),]
prev_day_WMS_Data_files <- prev_day_WMS_Data_files[!(hour(prev_day_WMS_Data_files$mtime) == 23),]
prev_day_GFS_files <- df[which(substr(rownames(df),22,35)== paste("GFS_",prev_date,sep = "")),]
prev_day_GFS_files <- prev_day_GFS_files[!(hour(prev_day_GFS_files$mtime) == 23),]

if (nrow(prev_day_WMS_Data_files) != 0) {
  if (nrow(prev_day_GFS_files) != 0) {
    if (Run_Time == 11) {
      #########worldweather.com_Hourly################
      Prev_Day_Weather <- read.csv(paste("/home/sunil/updates/WMS_Data_",prev_date,"_",max(hour(prev_day_WMS_Data_files$mtime)),".csv",sep = ""))
      Prev_Day_Weather$Date_DD_MM_YYYY <- prev_date
      colnames(Prev_Day_Weather)[1] <- "Hour"
      Prev_Day_Weather$Time <- NULL
      Prev_Day_Weather <- Prev_Day_Weather[,c(14,1:13)]
      WMS_raw_worldweather <- rbind(WMS_raw_worldweather,Prev_Day_Weather)
      
      ##########GFS Python_15mins#####################
      Prev_Day_GFS <- read.csv(paste("/home/sunil/updates/GFS_",prev_date,"_",max(hour(prev_day_WMS_Data_files$mtime)),".csv",sep = ""))
      Prev_Day_GFS$Radiation.gfs. <- Prev_Day_GFS$ghi+Prev_Day_GFS$dhi
      Prev_Day_GFS <- data.frame(Radiation.gfs. = Prev_Day_GFS[,11])
      Prev_Day_GFS$Date_DD_MM_YYYY <- as.factor(prev_date)
      Prev_Day_GFS$Time_Stamp <- as.factor(Time_Seq1$Time_Stamp)
      Prev_Day_GFS <- Prev_Day_GFS[,c(2,3,1)]
      WMS_raw_GFS <- rbind(WMS_raw_GFS,Prev_Day_GFS)
      
    }
  }
}



############Updating Weather & GFS Rawdata###############
write.csv(WMS_raw_worldweather,"/home/sunil/single_code/Raw_Data/WMS_raw_worldweather.csv",row.names = F)
write.csv(WMS_raw_GFS,"/home/sunil/single_code/Raw_Data/WMS_raw_GFS.csv",row.names = F)

Exist_Dates <- data.frame(Date_DD_MM_YYYY=unique(WMS_raw_worldweather$Date_DD_MM_YYYY))
Time_Seq <- data.frame(Time_Stamp = seq(ymd_hm('2015-01-01 00:00'),ymd_hm('2016-12-31 23:45'), by = '15 mins')[1:96])
Time_Seq$Time_Stamp <- times(strftime(as.character(Time_Seq$Time_Stamp), "%H:%M:%S"))
orig_Weather <- merge(Exist_Dates,Time_Seq,all.x = T)
orig_Weather <- orderBy(~Date_DD_MM_YYYY+Time_Stamp,orig_Weather)
row.names(orig_Weather) <- NULL
colnames(WMS_raw_worldweather)[2] <- 'Time_Stamp'
WMS_raw_worldweather$Time_Stamp <- times(strftime(as.POSIXct(as.character(WMS_raw_worldweather$Time_Stamp), format = "%H"), "%H:%M:%S"))
WMS_raw_worldweather <- merge(orig_Weather,WMS_raw_worldweather,all.x = T)
#WMS_raw_worldweather <- WMS_raw_worldweather[,c(1,2,10)]
WMS_raw_worldweather$Sunrise_time <- fillNAgaps(WMS_raw_worldweather$Sunrise_time)
WMS_raw_worldweather$Sunset_time <- fillNAgaps(WMS_raw_worldweather$Sunset_time)
WMS_raw_worldweather$Wind_Chill <- na.ma(WMS_raw_worldweather$Wind_Chill,k=4,weighting = "exponential")
WMS_raw_worldweather$Cloud_Cover_Percentage <- na.ma(WMS_raw_worldweather$Cloud_Cover_Percentage,k=4,weighting = "exponential")
WMS_raw_worldweather$Humidity_Percentage <- na.ma(WMS_raw_worldweather$Humidity_Percentage,k=4,weighting = "exponential")
WMS_raw_worldweather$Ambient_Temp <- na.ma(WMS_raw_worldweather$Ambient_Temp,k=4,weighting = "exponential")
WMS_raw_worldweather$Visibility <- fillNAgaps(WMS_raw_worldweather$Visibility)
WMS_raw_worldweather$Weather_Description <- fillNAgaps(WMS_raw_worldweather$Weather_Description)
WMS_raw_worldweather$Wind_Direction <- na.ma(WMS_raw_worldweather$Wind_Direction,k=4,weighting = "exponential")
WMS_raw_worldweather$Wind_Speed <- na.ma(WMS_raw_worldweather$Wind_Speed,k=4,weighting = "exponential")
WMS_raw_worldweather$Precipitation <- NULL
WMS_raw_worldweather$Pressure <- NULL
WMS_raw_worldweather$Sunrise_time <- as.POSIXct(WMS_raw_worldweather$Sunrise_time, format = "%I:%M %p")
WMS_raw_worldweather$Sunset_time <- as.POSIXct(WMS_raw_worldweather$Sunset_time, format = "%I:%M %p")

WMS_raw <- cbind(WMS_raw_worldweather,Radiation.gfs. = WMS_raw_GFS[,3])
WMS_raw$dummies_hour <- ifelse(chron::hours(WMS_raw$Time_Stamp) <=5, 0, ifelse(chron::hours(WMS_raw$Time_Stamp) <=10, 10, ifelse(chron::hours(WMS_raw$Time_Stamp) <=13, 10000,ifelse(chron::hours(WMS_raw$Time_Stamp) <=18, 10,0))))
WMS_raw$dummies_temp <- ifelse(WMS_raw$Ambient_Temp <25, 0, ifelse(WMS_raw$Ambient_Temp<28,500, ifelse(WMS_raw$Ambient_Temp < 32, 1000, 10000)))

Result <- merge(Mtr_raw,WMS_raw, by = c("Date_DD_MM_YYYY","Time_Stamp"))
Result <- orderBy(~Date_DD_MM_YYYY+Time_Stamp, data = Result)
row.names(Result) <- NULL

##############################Model##########################
Final <- Result
summary(Final)
Final$Season <- as.factor(ifelse(month(Final$Date_DD_MM_YYYY) >=6 & month(Final$Date_DD_MM_YYYY) <=9,"MONSOON",ifelse(month(Final$Date_DD_MM_YYYY) >=10 & month(Final$Date_DD_MM_YYYY)<=12,"POST_MONSOON",ifelse(month(Final$Date_DD_MM_YYYY)>=1 & month(Final$Date_DD_MM_YYYY)<=2, "WINTER","PRE_MONSOON"))))
#Final$Sunrise_Cat <- as.factor(ifelse(Final$Sunrise_time <=30, "TOO_EARLY",ifelse(Final$Sunrise_time <=35, "BIT_EARLY",ifelse(Final$Sunrise_time <=40,"EARLY",ifelse(Final$Sunrise_time <= "BIT_LATE","TOO_LATE")))))
#Final$Sunset_Cat <- as.factor(ifelse(Final$Sunset_time <= as.POSIXct("18:00:00",format="%H:%M:%S"),"TOO_EARLY_SS",ifelse(Final$Sunset_time <= as.POSIXct("18:30:00",format="%H:%M:%S"),"BIT_EARLY_SS",ifelse(Final$Sunset_time <= as.POSIXct("18:40:00",format="%H:%M:%S"),"LATE_SS",ifelse(Final$Sunset_time<= as.POSIXct("19:00:00",format="%H:%M:%S"),"BIT_LATE_SS","TOO_LATE_SS")))))

###########################################################
###################Location_1 Forecast######################
############################################################
model_vars <- c("Time_Stamp","Radiation.gfs.","dummies_hour","Ambient_Temp","Visibility","Weather_Description","Wind_Chill","Humidity_Percentage")
# model_vars <- c("Hour","Radiation.gfs.","Ambient_Temp","Humidity_Percentage" ,
#                   "Wind_Speed","Wind_Direction","dummies_hour",
#                   "Wind_Chill" ,"Precipitation","Cloud_Cover_Percentage",
#                   "Pressure","Visibility","Weather_Description", "Shortwave.Radiation")
farm1 = as.formula(paste("Active_Power_KW_1 ~ ", paste(model_vars, collapse= "+")))
################MAPE################
MAPE <- function(y, yhat)
{
  APCE<-abs((y - yhat)/y)
  if(length(which(is.infinite(APCE)|is.nan(APCE))))
    MAPE<-mean(APCE[-which(is.infinite(APCE)|is.nan(APCE))])
  if(!length(which(is.infinite(APCE)|is.nan(APCE))))
    MAPE<-mean(APCE)
  return(MAPE)
}
#################MAE################
MAE <- function(y,yhat)
{
  mean(abs(y-yhat))
}

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10
trc <-  trainControl(method = "repeatedcv",number = 10,repeats = 3,classProbs = TRUE)

#############Test####################
Prediction_Date <- as.Date(Sys.time(), format = "%Y-%m-%d") + 1
Prediction_Time <- as.character(Sys.time())
Prediction_Time <- substr(Prediction_Time, 12, 16)
temp = data.frame(timestamp = Sys.Date())
temp1=temp+1

###################read Test Data##################
#########worldweather.com_Hourly################
Current_Date <- as.Date(Sys.time(), format = "%Y-%m-%d")
weather <- read.csv("/home/sunil/single_code/Python/WMS_Data.csv")
weather$Date_DD_MM_YYYY <- Current_Date
Exist_Dates <- data.frame(Date_DD_MM_YYYY=unique(weather$Date_DD_MM_YYYY))
Time_Seq <- data.frame(Time_Stamp = seq(ymd_hm('2015-01-01 00:00'),ymd_hm('2016-12-31 23:45'), by = '15 mins')[1:96])
Time_Seq$Time_Stamp <- times(strftime(as.character(Time_Seq$Time_Stamp), "%H:%M:%S"))
orig_Weather <- merge(Exist_Dates,Time_Seq,all.x = T)
orig_Weather <- orderBy(~Date_DD_MM_YYYY+Time_Stamp,orig_Weather)
row.names(orig_Weather) <- NULL
weather$Time_Stamp <- times(strftime(as.POSIXct(as.character(weather$X), format = "%H"), "%H:%M:%S"))
test_weather <- merge(orig_Weather,weather,all.x = T)
test_weather <- test_weather[,c(1,2,4,5,7,9,12,13,14)]
test_weather$Ambient_Temp <- na.ma(test_weather$Ambient_Temp,k=4,weighting = "exponential")
test_weather$Sunrise_time <- fillNAgaps(test_weather$Sunrise_time)
test_weather$Sunrise_time <- as.POSIXct(test_weather$Sunrise_time, format = "%I:%M %p")
test_weather$Visibility <- fillNAgaps(test_weather$Visibility)
test_weather$Weather_Description <- fillNAgaps(test_weather$Weather_Description)
test_weather$Humidity_Percentage <- na.ma(test_weather$Humidity_Percentage,k=4,weighting = "exponential")
test_weather$Wind_Chill <- na.ma(test_weather$Wind_Chill,k=4,weighting = "exponential")
test_weather$Sunset_time <- fillNAgaps(test_weather$Sunset_time)
test_weather$Sunset_time <- as.POSIXct(test_weather$Sunset_time, format = "%I:%M %p")

###########WMS_Data 15mins interval writing to CSV#######
write.csv(test_weather,paste("/home/sunil/updates/WMS_15_mins/Test_Data_Weather_",Current_Date,"_",Run_Time,".csv",sep = ""))

##########GFS Python_15mins#####################
radiation <- read.csv("/home/sunil/single_code/Python/GFS.csv")
radiation$Radiation.gfs. <- radiation$ghi+radiation$dhi
radiation <- data.frame(Radiation.gfs. = radiation[,11])

test <- cbind(test_weather,radiation)
test$dummies_hour <- ifelse(chron::hours(test$Time_Stamp) <=5, 0, ifelse(chron::hours(test$Time_Stamp) <=10, 10, ifelse(chron::hours(test$Time_Stamp) <=14, 10000,ifelse(chron::hours(test$Time_Stamp) <=18, 10,0))))
test$dummies_temp <- ifelse(test$Ambient_Temp <25, 0, ifelse(test$Ambient_Temp<28,500, ifelse(test$Ambient_Temp < 32, 1000, 10000)))
test$Season <- as.factor(ifelse(month(test$Date_DD_MM_YYYY) >=6 & month(test$Date_DD_MM_YYYY) <=9,"MONSOON",ifelse(month(test$Date_DD_MM_YYYY) >=10 & month(test$Date_DD_MM_YYYY)<=12,"POST_MONSOON",ifelse(month(test$Date_DD_MM_YYYY)>=1 & month(test$Date_DD_MM_YYYY)<=2, "WINTER","PRE_MONSOON"))))
#test$Sunrise_Cat <- as.factor(ifelse(test$Sunrise_time <=30, "TOO_EARLY",ifelse(test$Sunrise_time <=35, "BIT_EARLY",ifelse(test$Sunrise_time <=40,"EARLY",ifelse(test$Sunrise_time <= "BIT_LATE","TOO_LATE")))))
#test$Sunset_Cat <- as.factor(ifelse(test$Sunset_time <= as.POSIXct("18:00:00",format="%H:%M:%S"),"TOO_EARLY_SS",ifelse(test$Sunset_time <= as.POSIXct("18:30:00",format="%H:%M:%S"),"BIT_EARLY_SS",ifelse(test$Sunset_time <= as.POSIXct("18:40:00",format="%H:%M:%S"),"LATE_SS",ifelse(test$Sunset_time<= as.POSIXct("19:00:00",format="%H:%M:%S"),"BIT_LATE_SS","TOO_LATE_SS")))))

test <- test[, model_vars]

################Train_Model#########################
set.seed(9)
train_data1 <- Final[1:nrow(Final),]
levels(train_data1$Active_Power_KW_1) <- make.names(levels(factor(train_data1$Active_Power_KW_1)))
mod1 <- train(farm1, data = train_data1, method = "knn",trControl=trc,tuneLength = 20)
pred_1st<-predict(mod1,test, type = "raw")
pred_1st_four_hour <- data.frame("Available_Capacity(AvC)MW" = "20", "Forecasted_Generation in MW" = pred_1st/1000)
pred_1st_four_hour$Forecasted_Generation.in.MW <- round(pred_1st_four_hour$Forecasted_Generation.in.MW,3)

Time_Seq <- data.frame("Blk No" = 1:96,From_Time = seq(ymd_hm('2015-01-01 00:00'),ymd_hm('2016-12-31 23:45'), by = '15 mins')[1:96])
Time_Seq$From_Time <- as.character(Time_Seq$From_Time)
Time_Seq$From_Time <- substr(Time_Seq$From_Time,12,16)
Time_Seq1 <- data.frame(To_Time = seq(ymd_hm('2015-01-01 00:15'),ymd_hm('2016-12-31 23:45'), by = '15 mins')[1:95])
Time_Seq1$To_Time <- as.character(Time_Seq1$To_Time)
Time_Seq1$To_Time <- substr(Time_Seq1$To_Time,12,16)
Time_Seq1[nrow(Time_Seq1)+1,] <- "00:00"
Pred_Time <- cbind(Time_Seq,Time_Seq1)
pred_1st_four_hour <- cbind(Pred_Time,pred_1st_four_hour)
# plot(tail(Final$Active_Power_KW_1/1000,96),type = "l",ylab = "Active_Power_KW",xlab = "Blk_No",ylim = c(0,8))
# par(new = T)
# plot(pred_1st_four_hour$Forecasted_Generation.in.MW,type = "l",col = "red",main = "Location_1",ylim = c(0,8),ylab = "",xlab = "")


###########################################################
###################Location_2 Forecast######################
############################################################
farm2 = as.formula(paste("Active_Power_KW_2 ~ ", paste(model_vars, collapse= "+")))

################Train_Model#########################
set.seed(9)
train_data1 <- Final[1:nrow(Final),]
levels(train_data1$Active_Power_KW_2) <- make.names(levels(factor(train_data1$Active_Power_KW_2)))
mod2 <- train(farm2, data = train_data1, method = "knn",trControl=trc,tuneLength = 20)
pred_2nd<-predict(mod2,test, type = "raw")
pred_2nd_four_hour <- data.frame("Available_Capacity(AvC)MW" = "20", "Forecasted_Generation in MW" = pred_2nd/1000)
pred_2nd_four_hour$Forecasted_Generation.in.MW <- round(pred_2nd_four_hour$Forecasted_Generation.in.MW,3)
pred_2nd_four_hour <- cbind(Pred_Time,pred_2nd_four_hour)

# plot(tail(Final$Active_Power_KW_1/1000,96),type = "l",ylab = "Active_Power_KW",xlab = "Blk_No",ylim = c(0,8))
# par(new = T)
# plot(pred_2nd_four_hour$Forecasted_Generation.in.MW,type = "l",col = "red",main = "Location_2",ylim = c(0,8),ylab = "",xlab = "")

Prediction_Loc1_Loc2 <- cbind(Pred_Time,Available_Capacity.AvC.MW = "20",Location_1 = pred_1st_four_hour[,5],Location_2 = pred_2nd_four_hour[,5])

write.csv(Prediction_Loc1_Loc2,"/home/sunil/Active_Power/Tomail/Prediction.csv",row.names = F)
write.csv(Prediction_Loc1_Loc2,paste("/home/sunil/Active_Power/updates/",Current_Date,"_",Run_Time,"th_Hour_Prediction.csv",sep = ""),row.names = F)

#pred_1st_four_hour$Forecasted_Generation.in.MW[1:2] <- ""
# write.csv(pred_1st_four_hour,"/home/sunil/single_code/R/Prediction.csv",row.names = F)
# write.csv(pred_1st_four_hour,paste("/home/sunil/updates/R/Prediction_",Current_Date,"_",Run_Time,".csv",sep = ""),row.names = F)
################writing to aws.s3####################
# setwd('/home/sunil/single_code/R/')
# Sys.setenv(AWS_ACCESS_KEY_ID = "XXXXXXXXXXX",AWS_SECRET_ACCESS_KEY = "XXXXXXXXXXXXXXXXXXX")
# put_object(file = "Prediction.csv", object = "TheObjectNameInsideS3", bucket = "YourBucketName")
