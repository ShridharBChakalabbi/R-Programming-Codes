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

# ABC Analysis
abc <- as.data.frame(unique(anu_Data$`Item Name`))
abc$'Amount' <- by(anu_Data$`Calculated Amount`,anu_Data$`Item Name`,sum)
abc <- abc[order(-abc$Amount),]
abc$'% of Total Amount' <- (abc$Amount/sum(abc$Amount))*100
abc$'Cumulative %' <- cumsum(abc$`% of Total Amount`)
abc$Grade <- ifelse(abc$`Cumulative %`<=81,"A",ifelse(abc$`Cumulative %`<=95,"B","C"))
x <- as.data.frame(c("A","B","C"))
colnames(x)[1] <- "Grades"
x$'Total Qty' <- by(abc$Grade,abc$Grade,length)
x$'Total Amount' <- by(abc$Amount,abc$Grade,sum)
x$'% of Total Quantity' <- (x$`Total Qty`/sum(x$`Total Qty`))*100
x$'% of Total Amount' <- (x$`Total Amount`/sum(x$`Total Amount`))*100
m <- as.matrix(t(x[,4:5]))
colnames(m) <- x$Grades
barplot(m,beside = T,legend.text = c('% of Total Quantity','% of Total Amount'),xlab = "Grades",ylab = "Percentage")

