

a <- Sys.time()
setwd("/home/mobiloans/mobiloans_r_codes")

#a<-data.frame(date=Sys.time)
#write.csv(a,"started.csv",row.names=F)

library(sqldf)
library(RMySQL)
library(plyr)
library(dplyr)
library(reshape2)
library(rpart)
library(RColorBrewer)
#install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
#library(rattle)
library(lubridate)

print("library loaded")
###############
################ to get the connection between sql and R
con = dbConnect(MySQL(), user='mobiloans', password='mobiloans123', dbname='mobiloans', host='34.214.211.162')
Listtables<-dbListTables(con)
options(sqldf.driver="SQLite")
print("connection established")
check <-dbGetQuery(con,paste("SELECT * FROM mobiloans.state_table"))

if((check$date[nrow(check)]==Sys.Date())&(check$state_machine_code[nrow(check)]=="C1"))
{

###############
################ Taking date with respect to the 1 day
seq_date1 <- Sys.Date()
seq_date2 <- seq_date1-15

#################### selecting auto and manual dialer 
###################  file according to the date 
Auto_dialer <-dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_auto_dialer where date(date)= '",seq_date1,"'"))
#Auto_dialer <- read.csv("20180321_Mobiloans_AutoDialer.CSV")
Auto_dialer$LastNSFDate <- as.Date(as.character(Auto_dialer$LastNSFDate))
Auto_dialer$NextActivityDate <- as.Date(as.character(Auto_dialer$NextActivityDate))
Auto_dialer$LastActivityTime <- as.Date(as.character(Auto_dialer$LastActivityTime))


manual_dialer <-dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_manual_dialer where date(date)='",seq_date1,"'"))
#manual_dialer <- read.csv("20180321_Mobiloans_ManualDialer.CSV")
manual_dialer$last_nsf_date <- as.Date(as.character(manual_dialer$last_nsf_date))
manual_dialer$nextActivityDate <- as.Date(as.character(manual_dialer$nextActivityDate))
manual_dialer$lastActivityTime <- as.Date(as.character(manual_dialer$lastActivityTime))

print("manual and auto dialer files readed from DB")
##################### combining the auto and manual dialer with respect
################# to the column names and all 
names(Auto_dialer)
names(manual_dialer)
Auto_dialer1 <- Auto_dialer[,-3] ### removing extra column
manual_dialer1 <- manual_dialer[,-c(21,22,23)] ### removing extra columns
names(manual_dialer1) <- names(Auto_dialer1) #####changing the manual dialer names
com <- rbind.fill(Auto_dialer1,manual_dialer1) #### complete data 

############# taking the payment files from the db
print("reading payment files")

pay <-dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_payment_file where 
                                        date(transaction_effective_date)>='",seq_date2,"' 
                           and date(transaction_effective_date)<='",seq_date1,"'"))

sql_payment <- dbGetQuery(con,paste("select loan_number as AccountNumber, count(transaction_amount) as Count_payment, sum(transaction_amount) as payment 
                                    from mobiloans.mobiloans_payment_file where transaction_type_description='Payment' 
                                    and date(transaction_effective_date)>='",seq_date2,"' 
                                    and date(transaction_effective_date)<= '",seq_date1,"' group by loan_number"))

###########preparing the payment files for analysis
print("preparing the payment files")
pay$reversal_transaction_id[pay$reversal_transaction_id==""] <- NA
pay$transaction_type_description <- as.character(pay$transaction_type_description)
pay$transaction_type_description <- sub("[[:space:]]+$","",pay$transaction_type_description)

pay1 <- pay[which(!is.na(pay$reversal_transaction_id)),]
pay1$transaction_amount <- as.numeric(pay1$transaction_amount)

s <- ddply(pay1,.(loan_number),summarise,rev_paid_amnt=sum(transaction_amount))
sql_payment$rev_paid_amnt <- s$rev_paid_amnt[match(sql_payment$AccountNumber,s$loan_number)]
if(nrow(s)==0){sql_payment$rev_paid_amnt <- 0}
sql_payment$actual_payment <- (sql_payment$payment - sql_payment$rev_paid_amnt)
sql_payment$actual_payment <- ifelse(is.na(sql_payment$actual_payment),as.character(sql_payment$payment),as.character(sql_payment$actual_payment))
#####################################################
################## data prep for auto and manual dialer
#####################
print("preparing manual and auto dialer files")
acc_num <- data.frame(AccountNumber=unique(com$AccountNumber))
#acc_num$Above_45 <- com$Above_45[match(acc_num$AccountNumber,com$AccountNumber)]
acc_num$AccountNumber <- as.numeric(as.character(acc_num$AccountNumber))
acc_num<- na.omit(acc_num)
####odd or even flag
for(i in 1:length(acc_num$AccountNumber)){if((acc_num$AccountNumber[i] %% 2) == 0) {
  acc_num$Even_odd[i] <- "even"} else {acc_num$Even_odd[i] <- "odd"}}

########## current date and last nsf date
com$date <- Sys.Date()
d2 <- ddply(com,.(AccountNumber),summarise,date = max(as.Date(date)))
d2 <- merge(d2,com,by=c("AccountNumber","date"))
NSF <- ddply(com,.(AccountNumber),summarise,Diff_NSF_date=difftime(seq_date1,max(LastNSFDate,na.rm = T),units = "days"),
             Diff_NextActivityDate=difftime(seq_date1,max(NextActivityDate,na.rm = T),units = "days"),
             Diff_LastActivityTime=difftime(seq_date1,max(LastActivityTime,na.rm = T),units = "days"))
NSF[NSF=="Inf"] <- 0

#########
acc_num$Current_balance <- d2$CurrentBalance[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$days_past_due <- d2$DaysPastDue[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$past_due_amount <- d2$PastDue[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$credit_limit <- d2$CreditLimit[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$action_code <- d2$ActionCode[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$Credit_exp_ratio <- as.numeric(as.character(acc_num$Current_balance))/as.numeric(as.character(acc_num$credit_limit))
acc_num <- merge(acc_num,NSF,by = "AccountNumber")
##########
acc_num$payment <- sql_payment$actual_payment[match(acc_num$AccountNumber,sql_payment$AccountNumber)]
acc_num$payment[is.na(acc_num$payment)] <- 0
acc_num$payment <- ifelse(as.numeric(acc_num$payment)>0,0,1)

acc_num$grt45 <- ifelse(as.numeric(as.character(acc_num$days_past_due))>=45,">45","<45")
#######################model input
########## less than 45 and odd numbers

test <- acc_num[acc_num$Even_odd=="odd" & acc_num$grt45=="<45",]
names(test)
test1 <- test[,c(3:6,8:12)]
test1 <- data.frame(sapply(test1,function(x) as.numeric(as.character(x))))
test1 <- cbind(test1,action_code=test$action_code)
test1$payment <- as.factor(test1$payment)
test1$action_code <- as.factor(test1$action_code)

###################
# test1$pred <- predict(fit,test1,type = "response")
# test1$pred_grp <- ifelse(test1$pred>=0.5,1,0)  
# x <- table(test1$payment,test1$pred_grp)
# sum(diag(x))/sum(x)
##########################
#setwd("C:/Users/admin/Desktop/21-03 model")
load("my_model1.rda")
#load("my_model2.rda")
# str(test1)
# 
# summary(rpart1)
# 
# 
# test1$pred <- (predict(fit1,test1,type="response"))
# test1$pred_grp <- ifelse(test1$pred>=0.2,1,0)
# x1 <- table(test1$payment,test1$pred_grp)
# prop.table(x1,1)
# y <- sum(diag(x1))/sum(x1)
# 
# ###########
# test1$AccountNumber <- test$AccountNumber
# ############
# 
# test1 <- test1[order(test1$pred,decreasing = T),]
# sl3 <- test1
# row.names(sl3) <- seq(1:nrow(sl3))
# sl3$top <- ifelse(as.numeric(row.names(sl3))<=300,"High_PPN","Low_PPN")

#############
print("predicting for the model")
test1$pred <- as.data.frame(predict(rpart1,test1,type="prob"))[2]
test1$predicted_gp <- ifelse(test1$pred>=0.3,1,0)
x <- table(test1$payment,test1$predicted_gp)
print(prop.table(x,1))
y <- sum(diag(x))/sum(x)

##############
#########################
print("preparing the output file")

test$pred <- test1$pred
test$predicted_gp <- test1$predicted_gp
test <- test[order(test$pred,decreasing = T),]
sl3 <- test
sl3$pred <- sl3$pred[,1]
sl3$predicted_gp <- sl3$predicted_gp[,1]
row.names(sl3) <- seq(1:nrow(sl3))
sl3$top <- ifelse(as.numeric(row.names(sl3))<=300,"Dont_call","call")
sl3$Model <- "Model"
##################
acc_num$pred <- sl3$pred[match(acc_num$AccountNumber,sl3$AccountNumber)]
acc_num$predicted_gp <- sl3$predicted_gp[match(acc_num$AccountNumber,sl3$AccountNumber)]
acc_num$result <- sl3$top[match(acc_num$AccountNumber,sl3$AccountNumber)]
acc_num$Model <- sl3$Model[match(acc_num$AccountNumber,sl3$AccountNumber)]
###########

acc_num$Auto_manual <- Auto_dialer$AccountNumber[match(acc_num$AccountNumber,Auto_dialer$AccountNumber)]
acc_num$Auto_manual[is.na(acc_num$Auto_manual)] <- "Manual_Dailer"
acc_num$Auto_manual <- ifelse(acc_num$Auto_manual!="Manual_Dailer","Auto_Dailer",acc_num$Auto_manual)
acc_num$result <- ifelse(is.na(acc_num$result),"call",acc_num$result)
acc_num$Model <- ifelse(is.na(acc_num$Model),"Control","Model")
acc_num$Aspen_Yessio <- Auto_dialer$dialer_company[match(acc_num$AccountNumber,Auto_dialer$AccountNumber)]
acc_num$Aspen_Yessio <- ifelse(is.na(acc_num$Aspen_Yessio),"yessio",acc_num$Aspen_Yessio)
acc_num$date_model <- Sys.Date()
###########
if(nrow(acc_num)<100){
 check$state_machine_code <- "C3"
 check$process_name <- "modeling_fail"
 dbWriteTable(con,"state_table",check,append=TRUE,overwrite=FALSE,row.names=FALSE)
}else{
 check$state_machine_code <- "C2"
 check$process_name <- "modeling_success"
 dbWriteTable(con,"state_table",check,append=TRUE,overwrite=FALSE,row.names=FALSE)
}



print(table(acc_num$Auto_manual,acc_num$Aspen_Yessio))
acc_num <- as.data.frame(sapply(acc_num,function(x) as.character(x)))
acc_num$date_model <- as.Date(acc_num$date_model)
print("writing to the DB")
dbWriteTable(con,"model_output_file",acc_num,append=TRUE,overwrite=FALSE,row.names=FALSE)
#########
#a<-data.frame(date=Sys.time)
#write.csv(a,"ended.csv",row.names=F)

setwd("/home/mobiloans/mobiloans_r_codes/model_output_csv")
write.csv(acc_num,paste(Sys.Date(),"account_number.csv",sep = ""),row.names = F)
b <- Sys.time()
print(difftime(b,a,units = "min"))
}else{print("No data for Modeling")}

