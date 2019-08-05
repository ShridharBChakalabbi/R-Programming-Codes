library(dplyr)
library(lubridate)
library(chron)
library(plyr)
library(ggpubr)
library(doBy)
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

mrg_Evg <- read.csv("/home/shridhar/Downloads/morning_evening.csv")
colnames(mrg_Evg)[2] <- "Time_Stamp"
mrg_Evg$Active.Power..KW. <- na.interpolation(mrg_Evg$Active.Power..KW.)
mrg_Evg$Wind_Chill <- na.ma(mrg_Evg$Wind_Chill,k=4,weighting = "exponential")
mrg_Evg$Cloud_Cover_Percentage <- na.ma(mrg_Evg$Cloud_Cover_Percentage,k=4,weighting = "exponential")
mrg_Evg$Humidity_Percentage <- na.ma(mrg_Evg$Humidity_Percentage,k=4,weighting = "exponential")
mrg_Evg$Ambient_Temp <- na.ma(mrg_Evg$Ambient_Temp,k=4,weighting = "exponential")
mrg_Evg$Visibility <- fillNAgaps(mrg_Evg$Visibility)
mrg_Evg$Weather_Description <- fillNAgaps(mrg_Evg$Weather_Description)
mrg_Evg$Wind_Direction <- na.ma(mrg_Evg$Wind_Direction,k=4,weighting = "exponential")
mrg_Evg$Wind_Speed <- na.ma(mrg_Evg$Wind_Speed,k=4,weighting = "exponential")
mrg_Evg$temp_dummies <- ifelse(mrg_Evg$Active.Power..KW.>22,"low_temp",ifelse(mrg_Evg$Active.Power..KW.>24,"medium_temp","High_temp"))

mrg_Evg$time <- ifelse(mrg_Evg$Hour<=6,"morning",ifelse(mrg_Evg$Hour<=7,"morning",ifelse(mrg_Evg$Hour<=8,"morning","Evening")))


#mrg_Evg$morn_even <- ifelse(mrg_Evg$Time_Stamp<=06:00:00,"morning",ifelse(mrg_Evg$Time_Stamp<=06:15:00,"morning",ifelse(mrg_Evg$Time_Stamp<=06:30:00,"morning",ifelse(mrg_Evg$Time_Stamp<=06:45:00,"morning",ifelse(mrg_Evg$Time_Stamp<=07:00:00,"morning",ifelse(mrg_Evg$Time_Stamp<=07:15:00,"morning",ifelse(mrg_Evg$Time_Stamp<=07:15:00,"morning",ifelse(mrg_Evg$Time_Stamp<=07:30:00,"morning",ifelse(mrg_Evg$Time_Stamp<=07:45:00,"morning",ifelse(mrg_Evg$Time_Stamp<=08:00:00,"morning","Evening"))))))))))
mrg_Evg <- Result
mrg_Evg$rad <-ifelse(mrg_Evg$Radiation.gfs.>40,"low_rad",ifelse(mrg_Evg$Radiation.gfs.>500,"medium_rad","High_rad")) 
#mrg_Evg <- Result
mrg_Evg$rad <- as.factor(mrg_Evg$rad)
mrg_Evg$Humidity <-as.factor(ifelse(mrg_Evg$Humidity_Percentage>30,"0",ifelse(mrg_Evg$Humidity_Percentage>90,"1","2"))) 
#mrg_Evg$morning_evening <- ifelse(mrg_Evg$Hour<=6,"morning",ifelse(mrg_Evg$Hour<=7,"morning",ifelse(mrg_Evg$Hour<=8,"morning","Evening")))
#mrg_MORN <- subset(mrg_Evg,morning_evening="morning")
#mrg_EVENING <- subset(mrg_Evg,morning_evening="Evening")
Evening <- subset(mrg_Evg,time=="Evening")
morning <- subset(mrg_Evg,time=="morning")
train <- mrg_Evg[1:1044,]
test <- mrg_Evg[1045:1144,]

model_vars <- c("Time_Stamp","Radiation.gfs.","temp_dummies","Ambient_Temp","Visibility","Weather_Description","Wind_Chill","Humidity_Percentage")
model_vars <- c("rad","Time_Stamp","Humidity_Percentage","Radiation.gfs.")
#,"Time_Stamp","Humidity_Percentage")
# model_vars <- c("Hour","Radiation.gfs.","Ambient_Temp","Humidity_Percentage" ,
#                   "Wind_Speed","Wind_Direction","dummies_hour",
#                   "Wind_Chill" ,"Precipitation","Cloud_Cover_Percentage",
#                   "Pressure","Visibility","Weather_Description", "Shortwave.Radiation")
farm1 = as.formula(paste("Active.Power..KW.~ ", paste(model_vars, collapse= "+")))
#farm1<-c("Active.Power..KW.")
farset.seed(9)
#train_data1 <- Final[1:nrow(Final),]
repeats = 3
numbers = 10
tunel = 10
trc <-  trainControl(method = "repeatedcv",number = 10,repeats = 3,classProbs = TRUE)

#############Test####################
#levels(train_data1$Active_Power_KW_2) <- make.names(levels(factor(train_data1$Active_Power_KW_2)))
mod2 <- train(farm1, data = train, method = "knn",trControl=trc,tuneLength = 20)
farpred_2nd<-data.frame(predict(mod2,test, type = "raw"))
Refr_KNN <- data.frame(Act=test$Active.Power..KW.,Pred =farpred_2nd,Time=test$Time_Stamp)
Refr_KNN$Accuracy <- (((test$Active.Power..KW.)-(Refr_KNN$predict.mod2..test..type....raw..))/(test$Active.Power..KW.))*100
##########################linear_regression##############################

mod4 <- lm(farm1,data=train)
ln_pred <- data.frame(predict(mod4,test))
Refr_Linear <- data.frame(Act=test$Active.Power..KW.,Pred =ln_pred,Time=test$Time_Stamp)
Accuracy <- (((test$Active.Power..KW.)-(ln_pred))/(test$Active.Power..KW.))*100
Refr_Linear$Accuracy <- Accuracy
#################Random_forest########################################
library(randomForest)
library(rpart)
library(dplyr)
library(caret)
data_fac=train %>% mutate_if(is.character, as.factor)
mod3 <- rpart(farm1,data=train)#,method = "class")
summary(mod3)
pred2 <- data.frame(predict(mod3,test))
refr_randomforest <- data.frame(Act=test$Active.Power..KW.,pred2,Time=test$Time_Stamp)
Accuracy <- (((test$Active.Power..KW.)-(pred2))/(test$Active.Power..KW.))*100
refr_randomforest$Accuracy <- Accuracy
Refr_full <- data.frame(KNN=Refr_KNN,Linear=Refr_Linear,Random_Forest=refr_randomforest)
morning <- subset(Refr,KNN.Time=="morning")
Evening <- subset(Refr,KNN.Time=="Evening")

write.csv(Refr,"/home/shridhar/Desktop/without_timeStamp.csv")
write.csv(Refr1,"/home/shridhar/Desktop/with_timestamp.csv")


sum1<-function(x,y)
{
  browser()
  add = x+y
  sub = x-y
  mul = x*y
  pow = power(x,y)
}

sum1(1,3)
