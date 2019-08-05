library(sqldf)
library(RMySQL)

con = dbConnect(MySQL(), user='mobiloansteam', password='team123456', dbname='mobiloans', host='34.214.211.162')
Listtables<-dbListTables(con)
options(sqldf.driver="SQLite")




setwd("E:\\(1) Mobiloans Dataset\\")
current_manual_dialler = read.csv("Current - Manual dialler 30jan18 to 6mar18.csv")
historical_manual_dialler = read.csv("Historical - Manual dialler.csv")
historical_manual_dialler_return = read.csv("Historical - Manual dialler return.csv")
current_PaymentsFile = read.csv("Current - PaymentsFile-31Dec2017 to 31Jan2018.csv")
current_auto_dialler = read.csv("Current - Auto dialler 29jan18to1mar18.csv")
historical_auto_dialler = read.csv("Historical - Auto dialler.csv")


length(unique(current_auto_dialler$AccountNumber%in%current_PaymentsFile$AccountNumber))
length(unique(current_PaymentsFile$AccountNumber))

m1 = merge(current_auto_dialler,current_PaymentsFile,by="AccountNumber")
#####################################################
library(plyr)
library(reshape2)

d1 <- ddply(current_auto_dialler,.(AccountNumber),summarise,NO_unsf=length(unique(LastNSFDate)))
d2 <- ddply(current_auto_dialler,.(AccountNumber),summarise,date = max(as.Date(date)))
d2 <- merge(d2,current_auto_dialler,by=c("AccountNumber","date"))
d3 <- ddply(current_auto_dialler,.(AccountNumber),summarise,date = min(as.Date(date)))
d3 <- merge(d3,current_auto_dialler,by=c("AccountNumber","date"))

d1$Max_date_current_bal <- d2$CurrentBalance[match(d1$AccountNumber,d2$AccountNumber)]
d1$Min_date_current_bal <- d3$CurrentBalance[match(d1$AccountNumber,d3$AccountNumber)]
d1$partly_paid <- d1$Min_date_current_bal-d1$Min_date_current_bal
d1$max_date_days_past_due <- d2$DaysPastDue[match(d1$AccountNumber,d2$AccountNumber)]

d4 <- dcast(current_auto_dialler,AccountNumber~ActionCodeDescription,length)
d1 <- merge(d1,d4,by = "AccountNumber")
d5 <- ddply(current_auto_dialler,.(AccountNumber),summarise,No_calls=length(AccountNumber))
d1$No_calls <- d5$No_calls[match(d5$AccountNumber,d2$AccountNumber)]
d1$max_date_ctionCodeDescription <- d2$ActionCodeDescription[match(d5$AccountNumber,d2$AccountNumber)]

d6 <- ddply(current_auto_dialler,.(AccountNumber),summarise,Max_current_bal=max(CurrentBalance))
d1$Max_current_bal <- d6$Max_current_bal[match(d1$AccountNumber,d6$AccountNumber)]


p <- ddply(current_PaymentsFile,.(AccountNumber,TransactionTypeDescription),summarise,summ=sum(TransactionAmount,na.rm = T))
p$TransactionTypeDescription <- as.character(p$TransactionTypeDescription)
p$TransactionTypeDescription <- sub("[[:space:]]+$","",p$TransactionTypeDescription)
unique(p$TransactionTypeDescription)
p1 <- p[which(p$TransactionTypeDescription=="Payment"),]
p2 <- p[p$TransactionTypeDescription=="Profit/Loss",]


d1$payment <- p1$summ[(match(d1$AccountNumber,p1$AccountNumber))]
d1$Profit_loss <- p2$summ[match(d1$AccountNumber,p2$AccountNumber)]
d1$Credit_limit <- current_auto_dialler$CreditLimit[(match(d1$AccountNumber,current_auto_dialler$AccountNumber))]

d1$credit_exp_ratio <- (d1$Max_current_bal-d1$payment)/d1$Credit_limit


payment <- NULL
uni_accnumber<-unique(current_auto_dialler$AccountNumber)
for(i in 1:length(uni_accnumber))
{
  aa <-current_auto_dialler[which(current_auto_dialler$AccountNumber == uni_accnumber[i]),]
  z=c(0,diff(aa$CurrentBalance))
  aa$partial_paid <- z
  payment <- rbind(payment,aa)  
  print(i)
}

loan <- payment[payment$partial_paid>0,]
l <- ddply(loan,.(AccountNumber),summarise,loan_amnt=sum(partial_paid))
d1$taken <- l$loan_amnt[match(d1$AccountNumber,l$AccountNumber)]
d1$loan_amount <- d1$Min_date_current_bal+d1$taken
d1$loan_amount <- ifelse(is.na(d1$loan_amount),d1$Min_date_current_bal,d1$loan_amount)

d1$credit_exp_ratio <- d1$Max_date_current_bal/d1$Credit_limit
d1$payment_cap <- d1$payment/d1$Credit_limit



sql_payment <-dbGetQuery(con,paste("select loan_number as AccountNumber,count(transaction_amount) as Count_payment,sum(transaction_amount) as payment from mobiloans.mobiloans_payment_file where transaction_type_description='Payment' group by loan_number"))
pay <- dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_payment_file"))
pay$reversal_transaction_id[pay$reversal_transaction_id==""] <- NA
pay1 <- pay[which(!is.na(pay$reversal_transaction_id)),]
names(pay1)
pay1$transaction_amount <- as.numeric(pay1$transaction_amount)
s <- ddply(pay1,.(loan_number),summarise,rev_paid_amnt=sum(transaction_amount))

sql_payment$rev_paid_amnt <- s$rev_paid_amnt[match(sql_payment$AccountNumber,s$loan_number)]
sql_payment$actual_payment <- sql_payment$payment-sql_payment$rev_paid_amnt
sql_payment$actual_payment <- ifelse(is.na(sql_payment$actual_payment),sql_payment$payment,sql_payment$actual_payment)


d1$payment <- sql_payment$actual_payment[match(d1$AccountNumber,sql_payment$AccountNumber)]
d1$payment <- ifelse(is.na(d1$payment),0,d1$payment)


d1$credit_exp_ratio <- d1$Max_date_current_bal/d1$Credit_limit
d1$payment_cap <- d1$payment/d1$Credit_limit

d1$qua <- ifelse(d1$credit_exp_ratio>=mean(d1$credit_exp_ratio) &d1$payment_cap>=mean(d1$payment_cap),"Cash_cows",
                 ifelse(d1$credit_exp_ratio>=mean(d1$credit_exp_ratio) &d1$payment_cap<=mean(d1$payment_cap),"Expendables",
                        ifelse(d1$credit_exp_ratio<=mean(d1$credit_exp_ratio) &d1$payment_cap<=mean(d1$payment_cap),"Sustainers","Promotable")))



d1$credit_group <- ifelse(d1$Credit_limit>1500,"1500",
                          ifelse(d1$Credit_limit>1200,"1200",ifelse(d1$Credit_limit>800,"800",
                                                                    ifelse(d1$Credit_limit>200,"200","0"))))

unique(d1$credit_group)
v1 <- d1[d1$credit_group=="0",]

table(d1$qua,d1$credit_group)

library(ggplot2)

ggplot(v1,aes(y = (credit_exp_ratio),x = (payment_cap)))+geom_point()+
  ylab("credit_exp_ratio=current_balance/credit_limit")+xlab("payment_cap=payment/credit_limit")+
  geom_vline(xintercept =mean(d1$credit_exp_ratio),col="red")+geom_hline(yintercept =mean(d1$payment_cap),col="red")+
  ggtitle(unique(paste("4 Quadrant Analysis Credit Limit = ",v1$credit_group,sep = "")))
plot(v1$payment_cap,v1$credit_exp_ratio)





