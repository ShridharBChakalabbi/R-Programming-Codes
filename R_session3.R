  
# Conditional statements

# If Else Statements

val<- 3

if (val>=0){
  print("postive number")
} else {
  print("negative number")
}


# Bonus applicable
# Greater than 1000 units - 200% bonus
# Between 801 and 1000 units - 100% bonus
# Between 501 and 800 units - 50% bonus
# Less than 500 units - no bonus


# Assume initial sales is 2000

sales<-2000

if (sales>1000) {
  print("Eligible for 200% bonus")
} else if(sales <=1000 & sales>800) {
  print("Eligible for 100% bonus")
} else if(sales<=800 & sales>500) {
  print("Eligible for 50% bonus")
} else{
  print("Not eligible for bonus")
}


?ifelse

# Create a new variable in the cars dataset
# called category and classify cars as 
# Very Fast (greater than 300 hp)
# Fast (between 201 and 300 hp)
# Medium (between 101 and 200 hp)
# Slow (less than 100 hp)
# Find the count of each category

a<-mtcars
a$category<-ifelse(a$hp<100,"Slow",
                   ifelse(a$hp>=100 & a$hp<200,"Medium",
     ifelse(a$hp>=200 & a$hp<300,"Fast","Very Fast")))
head(a)
class(a$category)

a$category<-as.factor(a$category)
class(a$category)

table(a$category)


# Another way of doing the above
b<-mtcars
b$category<-"Slow"
b$category[b$hp>100]<-"Medium"
b$category[b$hp>200]<-"Fast"
b$category[b$hp>300]<-"Very Fast"
View(b)
class(b$category)

b$category<-as.factor(b$category)
table(b$category)


# While Loops

ctrl<-1
while(ctrl<7) {
  print(paste("The ctrl is",ctrl))
  ctrl<-ctrl+1
}

speed<-90
while (speed>30) {
  print(paste("Speed is",speed))
  print ("Too fast, slow down!")
  speed<-speed -10
}
print(paste("Back to Normal Speed Limit"))


# For Loops
a=c(0,1,2,3,4,5)
for (i in a){
  print (paste('The count is:', i))
}


for (i in 1:10){
  sq = i*i
  print( sq)}

a=list(c(1,2,3,4,5), 5, c("A", "B"))
for (i in 1:length(a)){
  print(a[i])
}

#Loop modifications

# Breaking out of a loop prematurely
for (i in 1:10){
  sq = i*i
  print( sq)
  if (i >= 5){
    break
  }
}


#skipping iterations
for (i in 1:10){
  if (i%%2==0){
    next
  }
  sq = i*i
  print(c(i, sq))
}

------------------------------------------------------------------------------

# Functions

my_fun=function(){
  print ("hello")
}  

my_fun()

# Passing arguments
  
my_fun <- function(x,y) {
    (x*y)+(x/y) 
  }

my_fun(2,3)

my_fun(2)
  
a=2
b=3
my_fun(a,b)

a=c(1,2,3)
b=c(5,6,7)
my_fun(a,b)


# Creating default values in arguments


my_fun <- function(x,y=1) {
  z<-(x*y)+(x/y)
  print(x)
  print(y)
  return(z)
}

my_fun(2)

p = my_fun(2,3)
p

# Arguments by position and arguments by label

my_fun <- function(x,y=1,print_out=T) {
  a<-(x*y)
  b<-(x/y)
  z<-a+b
  if(print_out==T){
    print(paste("The final value is", z))
  } else {
    print("Print out not required")
    return (z)}
}

my_fun(2,3,F)

my_fun(2)

my_fun(2, T, 2 )

my_fun(y=2, print_out = T, x=2 )


#apply function 

#Apply function - used typically on matrices 


mat1 = matrix(1:9, nrow=3)
mat1

square = function(x){
  return(x*x)
}

apply(mat1, FUN=square, MARGIN=1)     
mat1

apply(mat1, FUN=mean, MARGIN =2)     
# margin = 2 means apply on each column
apply(mat1, FUN=mean, MARGIN =1)     
# margin = 1 means apply on each row


#lapply function - can be used on complex elements such as lists, dataframes etc
# function is applied column wise

list1 = list(c(10,20,30,35), 
             c(1,4,5,6), c(3,5,7,9))


lapply(list1, square)
lapply(list1, mean)

lapply(mat1, square)


#sapply function - similar to lapply but returns a simpler vector object instead of list
sapply(mat1, square)
sapply(list1, square)
sapply(list1, mean)


#using the apply functions to modify a dataframe

half = function(x){
  return (x/2)
}

temp=mtcars[1:5, 1:3]
temp


lapply(temp, half)
as.data.frame(lapply(temp, half))
class(lapply(temp, half))

class(sapply(temp, half))

as.data.frame(sapply(temp,half))