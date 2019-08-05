
# Factors


a<-c("Female","Male","Male","Male",
     "Female","Male","Female","Female")
class(a)

ab<-as.factor(a)
ab
class(ab)
levels(ab)

table(ab)



x=c(1,2,3,1,3,1,3,3,3,1,2)
x
class(x)
# assume that 1= Red, 2 = Green, 3 = Blue
colors=c("Red","Green", "Blue")
x=factor(x,labels=colors)
class(x)
x
table(x)






# to append specific labels to specific level values
x=c(1,2,3,1,3,1,3,3,3,1,2)
x=as.factor(x)
x
vals = c(3,1,2)
colors=c("Red","Green", "Blue")


x=factor(x, level)

x=factor(x,levels=vals, labels=colors)
class(x)
x
table(x)


x=c(1,2,3,1,3,2,1,1,3,2)
class(x)
sizes = c("S","M","L")
x=factor(x,labels=sizes, ordered=TRUE)
x
class(x)
table(x)

--------------------------------------------------------------------------------

# Lists

a=c(1,2,3)
b=c("A","B","C","D")
c=5
d=c(TRUE, FALSE)

x = list(a,b,c,d)
class(x)
x

# List slicing

x[[1]] = c(4,5,6)
x
x[4]

x[[4]][2]

x[[1]][2]

#modification of values in a list
x[[1]][2]=5    # Note: to reference a list member directly, use double square brackets
x[1]


#Named list members

a <- list (x=5, y=10, z=15)
a
a[2]
a$y



-----------------------------------------------------------------------

## Data frames


df<-data.frame(
  subj=c("math","sci","eng","hist"),
  mks1=c(20,22,15,18),mks2=c(20,22,15,18))

df
class(df)

colnames(df) = c("Subject", "Test1 Marks", "Test2 Marks")
rownames(df) = c("math","sci","eng","hist")

df

list1 = list(x= c(1,2,3,4,5), 
             y = c(10,20,30,40,50))
list1
df = as.data.frame(list1)
df
class(df)



#inbuilt datasets in R
data()


mtcars
class(mtcars)

help(mtcars)
?AirPassengers


## Useful functions when working with dataframes

# sample rows from dataframe
View(mtcars)

head(mtcars)
tail(mtcars)


head(mtcars,n=10)
tail(mtcars,15)

str(mtcars)
summary(mtcars)

nrow(mtcars)
ncol(mtcars)




## Calling columns in a dataframe 

mtcars[4]    # calling the 4th column (using index number)

mtcars$hp
mtcars[,4]
mtcars$gear

mtcars[1, 2]
mtcars["Mazda RX4", "cyl"]




## Subset the mtcars dataset for only mpg,cyl and hp


mtcars[,c("mpg","cyl","hp")]



## Subset the mtcars dataset for only cars having 
# more than 6 cylinders


mtcars[mtcars$cyl>6,]




## Subset function

subset(mtcars,cyl>6)


## Order function

a <- c(100, 10, 1000)
order(a)
sort(a)

# subset using logical operators
# & for AND condition
# | for OR condition
# ! for NOT condition

# Powerful cars have hp over 200 and have 6 or more cylinders
mtcars[mtcars$hp>200 & mtcars$cyl>=6,]

# I want to know the most powerful cars (over 300 hp) and the least 
# powerful cars (less than 100 hp)
mtcars[mtcars,hp > 300 & mtcars,hp < 100]
subset(mtcars,hp>300 | hp <100)


# Evaluate the expression below. Is the output TRUE or FALSE?
y<-5
z<-7
!(!(y<4) & !!!(z>12))


## Sort the mtcars dataset in descending order of hp

mt<-mtcars
mt[order(mtcars$hp,decreasing = T),]


# Sort the mtcars dataset in ascending order of
# column names

mt[,sort(names(mt))]


# Creating a new column
mt$new = mt$disp/mt$hp
mt$new = round(mt$new, 2)
mt

# adding a new row
mt["Verna",]=c(19.2,8 ,400.0 ,175 ,3.08, 3.845, 17.05,  0,  0,    3,   2, 2.29)

# merging two datasets

#1.Full merge - both the data sets get merged and all observations get displayed
#2.Inner Merge - the common data gets merged together to one new data set
#3.Left outer Merge - takes only left data set's observations
#4.Right outer Merge - takes only right data set's observations






#lets create two data sets
P = data.frame(Id = c(101,102,103,104), name = c("a","b","c","d"))
Q = data.frame(id1 = c(103,104,105,106), Age = c(24,25,23,34))
P
Q

#data2<merge(P,Q, by.x="ID", by.y = "id1", all,x = TRUE, all.y=TRUE)
# Full Merge
Data1<-merge(P, Q,by.x="Id",by.y="id1", all.x=TRUE, all.y=TRUE) # All = true 

Data1
View(Data1)

# Inner Merge  (skip the last argument of all=true)
merge(P, Q, by.x= "Id", by.y = "id1")


# Left Merge
merge(P, Q, by.x = "Id",by.y= "id1", all.x=TRUE)

#Right Merge
merge(P, Q, by.x = "Id",by.y= "id1",all.y=TRUE)


--------------------------------------------------------------------------------
  
#Handling Invalid data - NA, NaN, NULL, Inf
  
  # NA - Not Available
  # NaN - Not a Number
  # NULL - does not exist
  # Inf - Infinity

c(1/0, -1/0)


c(Inf+1, Inf-1, 1/Inf, Inf/100, Inf/Inf)


c(sqrt(-1), sqrt(Inf), 0/0)


c(log(Inf), log(0), exp(Inf), exp(-Inf))


x <- c(10,12,234,NA)
mean(x)

mean(x,na.rm=TRUE)



x <- c(10,12,234,NaN)
mean(x)

x <- c(10,15,20,NULL)
mean(x)

#class(x)
#class(NA)
#class(NaN)
#class(NULL)
#length(NA)
#length(NaN)
#length(NULL)


students<-list(Eric=95, Joseph=90, Sean=91,Eva=100)
students
students$Eva<-NULL
students$Eric<-NA
students

mt=mtcars
mt$new=mt$disp/mt$hp
mt
mt$new=NULL
mt
#Deleting null observations from a dataframe
df=as.data.frame(matrix(1:25, nrow=5))
df
#df1 = as.data.frame(matrix(1:25, nrow = 5))
df[c(1,3), c(2,4)]=NA

#df1[c(1,3), c(2,4)] = NA
df
df_new = na.omit(df)
df_new

-------------------------------------------------------------------------------

## Importing datasets

#text files
df = read.table(
  "C:/Users/Nagaraj/Documents/R-Training/State.txt",
           header = T,sep="\t", stringsAsFactors=F)
df1 - read.table("C:/Users/Nagaraj/Documents/R-Training/State.txt",header=T, sep ="\t", stringsAsFactors = F )
class(df)
str(df)


#Setting working directories

getwd()
setwd('C:/Users/Nagaraj/Documents/R-Training/')

df = read.table("State.txt",header = T,sep="\t",stringsAsFactors = F)
df


df1 = read.table("State.txt",header = F,
                 sep="\t",stringsAsFactors = F)
df1


df1 = read.table("State.txt",header = F,sep="\t",
                stringsAsFactors = F, 
         col.names=c("St", "Cap", "Pop", "Area"))
df1

`
df1 = read.table("titanic_test.csv",
          header = T,sep=",",stringsAsFactors = F)
df1

View(df)
str(df)
head(df1)
str(df1)

# changing data types within a dataframe
df1$Embarked = as.factor(df1$Embarked)
str(df1)

#reading in csv files with read.csv
df2 = read.csv("titanic_test.csv",
               stringsAsFactors = F)
str(df2)

df_temp=read.csv(file.choose())
View(df_temp)

#Reading in Excel Files
install.packages("readxl")
install.packages("xlsx")    # require internet access for this
library (xlsx)    
library(readxl)
# importing the installed package for use


sales<-read.xlsx("Excel_Trial.xlsx", 
                 sheetIndex = 1)
head(sales)
nrow(sales)
ncol(sales)

sales1<-read.xlsx
("Excel_Trial.xlsx", sheetIndex = 1, 
         startRow = 2,endRow = 7,colIndex=c(2,3),
       header = F, 
 colClasses = c("character", "character"))


head(sales1)
str(sales1)
nrow(sales1)
ncol(sales1)
colnames(sales1) = c("Sales", "Month")

rm (sales1)


# Writing to csv files
write.csv(sales1, "sales1_temp.csv")

#writing to excel files
write.xlsx(mtcars,'MTCars.xlsx')
write.xlsx(sales,"Excel_Trial.xlsx",
           sheetName = "Sales2",append=T)


