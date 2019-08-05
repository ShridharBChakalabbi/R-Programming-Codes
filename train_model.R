#setwd("~/Documents/mobi/21-03-18")
setwd("/home/mobiloans/mobiloans_r_codes")
a <- Sys.time()
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
con = dbConnect(MySQL(), user='mobiloansteam', password='team123456', dbname='mobiloans', host='34.214.211.162')
Listtables<-dbListTables(con)
options(sqldf.driver="SQLite")

print("connection established")
###############
################ Taking date with respect to the 60 days
seq_date1 <- Sys.Date()-1
seq_date2 <- seq_date1-15


#################### selecting auto and manual dialer 
###################  file according to the date 
Auto_dialer <-dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_auto_dialer where date(date)>=
                                   '",seq_date2,"' and date(date)<= '",seq_date1,"'"))

Auto_dialer$LastNSFDate <- as.Date(as.character(Auto_dialer$LastNSFDate))
Auto_dialer$NextActivityDate <- as.Date(as.character(Auto_dialer$NextActivityDate))
Auto_dialer$LastActivityTime <- as.Date(as.character(Auto_dialer$LastActivityTime))


manual_dialer <-dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_manual_dialer 
                                     where date(date)>= '",seq_date2,"' and date(date)<=  '",seq_date1,"'"))
manual_dialer$last_nsf_date <- as.Date(as.character(manual_dialer$last_nsf_date))
manual_dialer$nextActivityDate <- as.Date(as.character(manual_dialer$nextActivityDate))
manual_dialer$lastActivityTime <- as.Date(as.character(manual_dialer$lastActivityTime))
print("manual and auto dialer files readed from DB")
#####
##################### combining the auto and manual dialer with respect
################# to the column names and all 

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
##############

###############
################## building model for train
#names(acc_num)
train <- acc_num[,c(3:6,8:12)]
train <- data.frame(sapply(train,function(x) as.numeric(as.character(x))))
train <- cbind(train,action_code=acc_num$action_code)
train$payment <- as.factor(train$payment)
train$action_code <- as.factor(train$action_code)
names(train)
train1 <- train[,-c(5,10)]
#names(train1)
# ##############model
# 
# fit <- glm(payment~.,data = train,family = "binomial")
# summary(fit)
# library(car)
# vif(fit)
# #####
# fit <- glm(payment~.,data = train1,family = "binomial")
# summary(fit)
# library(car)
# vif(fit)
# fit1 <- step(fit)
# summary(fit1)
# vif(fit1)
# # 
# train$predicted <- predict(fit,train,type = "response")
# train$predicted_grp <- ifelse(train$predicted>=0.3,1,0)
# x <- table(train$payment,train$predicted_grp)
# prop.table(x,1)
# sum(diag(x))/sum(x)
# 
#######################
##########
print("model is building")

rpart1 <- rpart(payment ~.,data = train1)

dtte_predicted_y <- as.data.frame(predict(rpart1,train,type="prob"))
dtte_predicted_gp <- ifelse(dtte_predicted_y[2]>=0.3,1,0)
x <- table(train$payment,dtte_predicted_gp)
print(prop.table(x,1))
y <- sum(diag(x))/sum(x)
print(y)

b <- Sys.time()
# model_name=gsub("-","_",paste0(b,"_cost_model1.rda"))
save(rpart1, file = "my_model1.rda")



print(paste("program Execution time is ",difftime(b,a,units = "min")))
###########################
