setwd("/home/ubuntu/45days_model")


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
con = dbConnect(MySQL(), user='mobiloansteam', password='team123456', dbname='mobiloans_dev', host='52.41.219.149')
Listtables<-dbListTables(con)
options(sqldf.driver="SQLite")
print("connection established")


seq_date1 <- Sys.Date()-50
seq_date2 <- seq_date1+45
seq_date3 <- seq_date1-15
  
#################### selecting auto and manual dialer 
###################  file according to the date 
Auto_dialer <-dbGetQuery(con,paste("SELECT * FROM mobiloans_dev.mbl_auto_dialer where date(date)<= '",seq_date1,"' and date(date)>= '",seq_date1-6,"'"))
#Auto_dialer <- read.csv("20180321_Mobiloans_AutoDialer.CSV")
  
Auto_dialer[Auto_dialer==""] <- NA
Auto_dialer$LastNSFDate <- as.Date(Auto_dialer$LastNSFDate,"%m/%d/%Y")
Auto_dialer$NextActivityDate <- as.Date(Auto_dialer$NextActivityDate,"%m/%d/%Y")
Auto_dialer$LastActivityTime <- as.Date(Auto_dialer$LastActivityTime,"%m/%d/%Y")
  
  
manual_dialer <-dbGetQuery(con,paste("SELECT * FROM mobiloans_dev.mbl_manual_dialer where date(date)<='",seq_date1,"' and date(date)>='",seq_date1-6,"'"))
manual_dialer[manual_dialer==""] <- NA
manual_dialer$last_nsf_date <- as.Date(manual_dialer$last_nsf_date,"%m/%d/%Y")
manual_dialer$nextActivityDate <- as.Date(manual_dialer$nextActivityDate,"%m/%d/%Y")
manual_dialer$lastActivityTime <- as.Date(manual_dialer$lastActivityTime,"%m/%d/%Y")
  
print("manual and auto dialer files readed from DB")
  ##################### combining the auto and manual dialer with respect
  ################# to the column names and all 
  Auto_dialer1 <- Auto_dialer[,-3] ### removing extra column
  manual_dialer1 <- manual_dialer[,-c(21,22,23)] ### removing extra columns
  names(manual_dialer1) <- names(Auto_dialer1) #####changing the manual dialer names
  com <- rbind.fill(Auto_dialer1,manual_dialer1) #### complete data 
  
  ######### 15 days payment file #############
  
  print("reading payment files of previous 15 days")
  
  pay <-dbGetQuery(con,paste("SELECT * FROM mobiloans_dev.mbl_payment_file where 
                             date(transaction_effective_date)<='",seq_date1,"' 
                             and date(transaction_effective_date)>='",seq_date3,"'"))
  
  sql_payment <- dbGetQuery(con,paste("select loan_number as AccountNumber, count(transaction_amount) as Count_payment, sum(transaction_amount) as payment 
                                      from mobiloans_dev.mbl_payment_file where transaction_type_description='Payment' 
                                      and date(transaction_effective_date)<='",seq_date1,"' 
                                      and date(transaction_effective_date)>= '",seq_date3,"' group by loan_number"))
  
###########preparing the payment files for analysis
print("preparing the payment files of previous 15 days")
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
sql_payment$rev_paid_amnt <- ifelse(is.na(sql_payment$rev_paid_amnt),0,sql_payment$rev_paid_amnt)
sql_payment_15 <- sql_payment
##############################
  
  
print("preparing manual and auto dialer files")
acc_num <- data.frame(AccountNumber=unique(com$AccountNumber))
#acc_num$Above_45 <- com$Above_45[match(acc_num$AccountNumber,com$AccountNumber)]
acc_num$AccountNumber <- as.numeric(as.character(acc_num$AccountNumber))
acc_num<- na.omit(acc_num)
####odd or even flag
for(i in 1:length(acc_num$AccountNumber)){if((acc_num$AccountNumber[i] %% 2) == 0) {
acc_num$Even_odd[i] <- "even"} else {acc_num$Even_odd[i] <- "odd"}}
###############################
com$date <- Sys.Date()
d2 <- ddply(com,.(AccountNumber),summarise,date = max(as.Date(date)))
d2 <- merge(d2,com,by=c("AccountNumber","date"))
  
#########################
acc_num$Current_balance <- d2$CurrentBalance[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$days_past_due <- d2$DaysPastDue[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$past_due_amount <- d2$PastDue[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$credit_limit <- d2$CreditLimit[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$action_code <- d2$ActionCode[match(acc_num$AccountNumber,d2$AccountNumber)]
acc_num$Credit_exp_ratio <- as.numeric(as.character(acc_num$Current_balance))/as.numeric(as.character(acc_num$credit_limit))
###########################
acc_num$count_payment_15 <- sql_payment$Count_payment[match(acc_num$AccountNumber,sql_payment$AccountNumber)]
acc_num$payment_15 <- sql_payment$actual_payment[match(acc_num$AccountNumber,sql_payment$AccountNumber)]
################### next 45 days payment
  
print("reading payment files next 45 days")
  
pay <-dbGetQuery(con,paste("SELECT loan_number,sum(transaction_amount) as rev_pay 
                             FROM mobiloans_dev.mbl_payment_file where 
                             reversal_transaction_id!='' and date(transaction_effective_date)<='",seq_date2,"' 
                             and date(transaction_effective_date)>= '",seq_date1,"' group by loan_number"))
  
sql_payment <- dbGetQuery(con,paste("select loan_number as AccountNumber, count(transaction_amount) as Count_payment, sum(transaction_amount) as payment 
                                      from mobiloans_dev.mbl_payment_file where transaction_type_description='Payment' 
                                      and date(transaction_effective_date)<='",seq_date2,"' 
                                      and date(transaction_effective_date)>= '",seq_date1,"' group by loan_number"))
  
###########preparing the payment files for analysis
  
sql_payment$rev_paid_amnt <- pay$rev_pay[match(sql_payment$AccountNumber,pay$loan_number)]
if(nrow(s)==0){sql_payment$rev_paid_amnt <- 0}
sql_payment$actual_payment <- (sql_payment$payment - sql_payment$rev_paid_amnt)
sql_payment$actual_payment <- ifelse(is.na(sql_payment$actual_payment),as.character(sql_payment$payment),as.character(sql_payment$actual_payment))
  
################################
acc_num$payment_nxt <- sql_payment$actual_payment[match(acc_num$AccountNumber,sql_payment$AccountNumber)]
acc_num$payment_nxt[is.na(acc_num$payment_nxt)] <- 0
acc_num$payment_nxt <- ifelse(as.numeric(acc_num$payment_nxt)>0,0,1)
acc_num[is.na(acc_num)] <- 0
names(acc_num)
#####################

new <- acc_num[,c(3:6,8:11)]
new <- data.frame(sapply(new, function(x) as.numeric(x)))
new$payment_nxt <- as.factor(new$payment_nxt)
write.csv(new,paste(seq_date1,"train_dataset_dataset_new_week_wise.csv",sep = "_"),row.names = F)

training_set <- new
training_set[-8] = scale(training_set[-8])

library(h2o)
h2o.init(nthreads = -1)
model = h2o.deeplearning(y = 'payment_nxt',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(5,3),
                         epochs = 100,
                         train_samples_per_iteration = -1)

summary(model)

# Predicting the Test set results
pred = h2o.predict(model, newdata = as.h2o(training_set[-8]))
training_set$pred <- as.vector(pred[3])
training_set$y_pred = ifelse(training_set$pred>=0.3,1,0)

# Making the Confusion Matrix
cm = table(training_set$payment_nxt,training_set$y_pred)
cm
print(prop.table(cm,1))

h2o.saveModel(object=model, path=getwd(), force=TRUE)

