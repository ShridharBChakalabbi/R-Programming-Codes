# R programming language
---------------------------------------------
  
  
  
# Simple calculations
  
  
1+2

3*6

9/3

3^2

3+5+6

3-4-5

sum(3, 4, 5)
sum(1,2,3)


(1+2+3)/2

sqrt(4)

log(4)

exp(4)
2.718^4
1:100
5:10

#Help command
?log
?sum
?rbind
?cbind    

--------------------------------
  
# Creating objects

# Assignment operators

# <-

# =

# ->

a<-1+2
a
b<-3*6
b
c<-9/3

d<-3^2

e<-sum(3+5+6)

sum(1,2,3)->f
f

g<-(3+4+5)/2
g


h<-sqrt(4)
h
h=sqrt(10)
a<-b<-6
a
b

a=b=8
a
b

a=b<-9
a
b

a<-b=3
a
b



# Naming conventions

_acd<-3
.acd<-3
.acd
1.eds<-3
acd-asd<-3


a=3.5
class(a)

a=3L
class(a)

a="R-training"
class(a)




# Assign the Marks for one student for the 
# following subjects (Max marks= 25)
# Subject: math,sci,eng,hist






math<- 20
sci<- 22
eng<-15
hist<-18L
class(hist)
class(math)







# Find the sum total of all marks for the student







tot<-math+sci+eng+hist
tot
tot<-sum(math,sci,eng,hist)
tot













class(tot)


subj<-"Math"
class(subj)


logic<-F
class(logic)


comp<-4+2i
class(comp)



a=1.5
class(a)

is.integer(a)
is.numeric(a)
is.complex(a)

b=as.integer(a)
b
class(b)

c=as.character(a)
c

d="1.5"
class(d)
e=as.numeric(d)
d
e


## Data Objects





# Atomic Vector

a<-5


# c - combine, concatenate


a<-c(5,6,7)
a

class(a)

b=c("mpg", "disp")
class (b)

#coercion - forcible change of data type to avoid data type mismatch

new<-c(1,"new",TRUE)
new                          # all elements of vector have been changed to char
class(new)


c=c(3, 3L)        # All elements have been changed to numeric
class(c)

#Defining names / headers for elements of the vector
a=c(1,2,3)
names(a)
names(a)<-c("A","B","C")
names(a)
a



# Create a vector of marks of test one for a student


mks1<-c(25,22,21,18)
mks1

subj<-c("math","sci","eng","hist")
subj

names(mks1)<-subj
mks1



attributes(mks1)



mks1




# Create a vector of marks of test 2 for the same student


mks2<-c(20,22,15,18)
names(mks2)<-subj


mks1
mks2

tot<- mks1+mks2
tot

avg.tot<-tot/2
avg.tot

sum(tot)
sum(avg.tot)   # gives the sum of all the elements in the vector
length(tot)    # gives the total number of elements in the vector
tot

sum(avg.tot)/length(avg.tot)

sum(mks1)/length(mks1)


#Subsetting a vector

mks1
mks1[3]           # get value of third element (index =3)
mks1["math"]      # get value of math (referring using header name as against index number)

mks1[2]
mks1[c(2,4)]      # getting values for 2nd and 4th element
mks1 = mks1[-c(1,3)]   # removing 1st and 3rd elements and giving output of 2nd and 4th

mks1

## Subset the total marks vector for only math and eng

tot[c(1,3)]
tot[c("math","eng")]

tot

## Subset the total marks vector for sci,eng and hist


tot[c(2:4)]
tot[-3]
tot




## Comparison operators

## Subset the total marks for subjects where the 
# student has scored 40 and above marks



tot
tot>=40


high.mks<-tot>=40
high.mks
class(high.mks)
tot <40
tot[tot<40]
tot[high.mks==TRUE]

# vector math (element wise operations)

a=c(1,2,3)
b=c(2,3,4)

a+b
a*b

# recycling
a=c(1,2,3)
b=c(1,2,3,4,5,6)
d=c(1,2,3,4,5)

a+b      # note: smaller vector is repeated till it matches the length of the longer vector
a+d      # recycling happens but warning message will be given that longer vector is not a multiple of smaller vector.. so full recycling of smaller vector has not happened










## Matrices

m<-matrix(1:9,nrow=3)
m

m1<-matrix(1:10, nrow=2, ncol=5)
m1   

m1<-matrix(1:10, nrow=2, ncol=3)  # when size of matrix (ncol*nrow) < number of elements, all elements are not added to the matrix
m1

m1<-matrix(1:10, nrow=5, ncol=3)  # when size of matrix (ncol*nrow) > number of elements, elements are repeated till size = num of elements
m1

# population of matrix is automatically by column unless byrow option is specified as TRUE
m2<-matrix(1:10, nrow=5, ncol=2, byrow=TRUE)
m2


## Assigning row and column names to a matrix
m
rownames(m)<-c("A","B","C")
m
colnames(m)<-c("X","Y","Z")
m


# Accessing elements of a matrix
m
m[2,3]

m[2,]
m[,2]

m[ ,c(1,2)]

m[c("A","C"), c("X","Y")]


## rbind and cbind function to combine two vectors / matrices 
tot
tot.m<-rbind(mks1,mks2)   # rbind - combine by row (mks1, mks2 are row headers)
tot.m

tot.m2<-cbind(mks1,mks2)  # cbind - combine by col (mks1, mks2 are col headers)
tot.m2

class(tot.m)

## Row and Column sums function
tot.m
rowSums(tot.m)   # sum of a row (across columns)
colSums(tot.m)   # sum of a column (across rows)


## Add and name the row sums and column sums 
# to the tot.m matrix

tot.m3<-cbind(tot.m,rowSums(tot.m))
tot.m3

tot.m3<-rbind(tot.m3,colSums(tot.m3))
tot.m3




colnames(tot.m3)<-c(subj,"Colsum")
rownames(tot.m3)<-c("mks1","mks2","Rowsum")
tot.m3



## Subset the matrix for only math marks for both test
tot.m3
tot.m3[,1]
tot.m3[,"math"]


## Subset the matrix for only math and eng marks for 
# both tests

tot.m3[,c(1,3)]


## Subset the matrix for only math and eng marks
# for test 2


tot.m3[2,c(1,3)]




# Create a matrix for marks out of 100 

tot.m4<-tot.m3*4  # scalar multiplication
tot.m4


# Create 2 matrices 2x2


m1<-matrix(1:4,nrow=2)

m2<-matrix(1:4,nrow=2,byrow = TRUE)
m1
m2


# Element wise Multiplication

m1*m2

# Mathematical Matrix Multiplication
m1 %*% m2


# combining two matrices using rbind and cbind

# use cbind only if number of rows is the same for both matrices
# use rbind only if number of columns is the same for both matrices

m1= matrix(1:10, nrow=5)
m1

m2=matrix(21:30, nrow=5)
m2

m3=rbind(m1,m2)
m3

m4=cbind(m1,m2)
m4