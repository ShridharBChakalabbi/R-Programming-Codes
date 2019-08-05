# Set Working Directory
library(readxl)
library(ggplot2)
# Data Preparation
anu_Data <- read_excel("Anuprovisional.xlsx",sheet = 1)
anu_Data <- anu_Data[order(-anu_Data$Rate),]
anu_Data$Rate <- ifelse(anu_Data$Rate==111899,11,anu_Data$Rate)
anu_Data <- anu_Data[order(-anu_Data$Qty),]
anu_Data$Qty <- ifelse(anu_Data$Qty==111899,11,anu_Data$Qty)
anu_Data <- anu_Data[order(-anu_Data$Amount),]
anu_Data$Amount[14] <- 1
anu_Data$Amount <- ifelse(anu_Data$Amount==111899,11,anu_Data$Amount)
anu_Data <- anu_Data[order(anu_Data$`Item Name`),]
anu_Data$'Calculated Amount' <- anu_Data$Rate * anu_Data$Qty

# RFM Analysis
rfm <- aggregate(anu_Data$`date time`,by=anu_Data["Item Name"],FUN=max)
colnames(rfm)[2] <- 'Max of Date'
rfm$'Max of Date' <- as.Date(rfm$`Max of Date`)
max_date <- as.Date(max(anu_Data$`date time`))
rfm$'R' <- as.vector((max_date - rfm$`Max of Date`) + 1)
rfm$'F' <- as.vector(by(anu_Data$Qty,anu_Data$`Item Name`,sum))
rfm$'M' <- as.vector(by(anu_Data$`Calculated Amount`,anu_Data$`Item Name`,sum))
R_Thresholds <- as.vector(quantile(rfm$R,c(0.2,0.4,0.6,0.8,1)))
F_Thresholds <- as.vector(quantile(rfm$F,c(0.2,0.4,0.6,0.8,1)))
M_Thresholds <- as.vector(quantile(rfm$M,c(0.2,0.4,0.6,0.8,1)))
rfm$'R Points' <- ifelse(rfm$R<=R_Thresholds[1],5,ifelse(rfm$R<=R_Thresholds[2],4,ifelse(rfm$R<=R_Thresholds[3],3,ifelse(rfm$R<=R_Thresholds[4],2,1))))
rfm$'F Points' <- ifelse(rfm$F<=F_Thresholds[1],1,ifelse(rfm$F<=F_Thresholds[2],2,ifelse(rfm$F<=F_Thresholds[3],3,ifelse(rfm$F<=F_Thresholds[4],4,5))))
rfm$'M Points' <- ifelse(rfm$M<=M_Thresholds[1],1,ifelse(rfm$M<=M_Thresholds[2],2,ifelse(rfm$M<=M_Thresholds[3],3,ifelse(rfm$M<=M_Thresholds[4],4,5))))
rfm$'RFM Score' <- rfm$`R Points` + rfm$`F Points` + rfm$`M Points`
rfm <- rfm[order(-rfm$`RFM Score`),]
pl <- ggplot(rfm,aes(x=rfm$`RFM Score`))
pl + geom_bar()
x <- aggregate(rfm$`RFM Score`,by=rfm["RFM Score"],FUN=length)