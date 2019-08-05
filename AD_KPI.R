library(sqldf)
library(RMySQL)
library(plyr)
library(dplyr)
library(timeSeries)

con = dbConnect(MySQL(), user='mobiloansteam', password='team123456', dbname='mobiloans', host='34.214.211.162')
Listtables<-dbListTables(con)
options(sqldf.driver="SQLite")

d <- as.Date(timeSequence(from='2018-07-07',to = '2018-07-28',by="weeks"))
d

final=NULL

for (i in 1:(length(d)-2))
{  
  manual_dialer <-dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_manual_dialer 
                                       where date(date)>='",d[i],"' and date(date) <'",d[i+1],"'"))
  
  old_man <- dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_manual_dialer 
                                    where date(date) >= '",d[i]-7,"' and date(date) < '",d[i+1]-7,"'"))
  
  manual_dialer$str_account_number <- as.numeric(manual_dialer$str_account_number)
  old_man$str_account_number <- as.numeric(old_man$str_account_number)
  new <- manual_dialer
  new$new <- old_man$str_account_number[match(new$str_account_number,old_man$str_account_number)]
  new <- new[is.na(new$new),-ncol(new)]
  if(nrow(new)!=0)
  {
    pay <- dbGetQuery(con,paste("SELECT * FROM mobiloans.mobiloans_payment_file where date(transaction_effective_date) >='",d[i+1],"' and date(transaction_effective_date) <'",d[i+2],"'"))
    
    new$str_account_number <- as.numeric(new$str_account_number)
    md <- na.omit(new)
    md$str_account_number <- gsub(".*:","",md$str_account_number)
    
    pay$transaction_amount <- as.numeric(pay$transaction_amount)
    pay$loan_number <- gsub(".*:","",pay$loan_number)
    pay$transaction_type_description <- trimws(pay$transaction_type_description)
    #names(pay)
    
    pay2 <- sqldf("select loan_number as accountnumber, count(transaction_amount) as count_of_payment,sum(transaction_amount) as payment from pay where transaction_type_description='Payment' and transaction_code in (121,127,135) group by loan_number")
    pay$reversal_transaction_id[pay$reversal_transaction_id==""] <- NA
    pay$transaction_type_description <- as.character(pay$transaction_type_description)
    pay$transaction_type_description <- sub("[[:space:]]+$","",pay$transaction_type_description)
    
    if(nrow(pay[which(!is.na(pay$reversal_transaction_id)),])>=1)
    {
      p2 <- pay[which(!is.na(pay$reversal_transaction_id)),]
      p2$transaction_amount <- as.numeric(p2$transaction_amount)
      rv <- ddply(p2,.(loan_number),summarise,rev_paid_amount=sum(transaction_amount))
    } else
    {
      pay$transaction_amount <- as.numeric(pay$transaction_amount)
      rv <- ddply(pay,.(loan_number),summarise,rev_paid_amount=sum(transaction_amount))
    }
    #pm <- ddply(pay,.(loan_number),summarise,actual_payment=sum(transaction_amount))
    
    pay2$rev_paid_amount <- rv$rev_paid_amount[match(pay2$accountnumber,rv$loan_number)]
    if(nrow(rv)==0){pay2$rev_paid_amount <- 0}
    pay2$actual_payment <- pay2$payment - pay2$rev_paid_amount 
    pay2$actual_payment <- ifelse(is.na(pay2$actual_payment),as.character(pay2$payment),as.character(pay2$actual_payment))

    uac <- data.frame(accountnumber=unique(as.numeric(md$str_account_number))) ### Uniq A/C ######
    maxdate <- ddply(md,.(str_account_number),summarise,date = max(as.Date(date)))
    max_r <- merge(maxdate,md[,c(1,2,8)],by=c("str_account_number","date"))
    colnames(max_r)[1] <-"accountnumber" 
    f <- merge(uac,pay2,by="accountnumber")
    f[is.na(f)] <- 0
    #fs <- ddply(f,.(accountnumber),summarise,actual_payment=sum(as.numeric(actual_payment)))
    kpi <- data.frame(month =paste(d[i]),gross_collected=sum(f$payment),net_collected=sum(as.numeric(f$actual_payment)),reversed_amount=sum(f$payment)-sum(as.numeric(f$actual_payment)),total_amount_due=sum(as.numeric(max_r$AmountDue)))
    kpi$percentage_collected <- (kpi$net_collected/kpi$total_amount_due)*100
    kpi$calls_made <- nrow(md)
    kpi$no_of_uniq_acc <- length(unique(as.numeric(md$str_account_number)))
    kpi$no_of_acc_paid <- nrow(f)
    beg_ac <- md[which(md$date==min(as.Date(md$date))),]
    kpi$no_beg_ac <-length(unique(beg_ac$str_account_number))
    last_ac <- md[which(md$date==max(as.Date(md$date))),]
    kpi$no_last_ac <- length(unique(last_ac$str_account_number))
    settler <- md[which(as.numeric(md$days_past_due)>56),]
    kpi$no_settler <- length(unique(settler$str_account_number))
    
    final <- rbind(final,kpi)
  }
  print(d[i])
}
dbDisconnect(con)
setwd("E://")
write.csv(final,"MD_tillApril_thisweek_nextweek.csv",row.names = FALSE)
