

#Biostatistics Exercise 3#

#Question 1: create a function that calculates the quotient of the maximum and minimum values in a vector

##Question 1A THIS WORKED
maxmin1 = function(v){
q = (max(v)/min(v))
return(q)
}

# apply above function to rows of MaxOverMin.txt
maxminA <- apply(maxovermin,1,maxmin1) # apply takes maxmin1 as an argument

#Question 1B
#Adjust maxmin function to allow for handling of NA
#load data
maxminB <- read.table("MaxOverMinB.txt", header = FALSE)

#this one works
maxmin1b = function(x, na.rm = FALSE){ 
  if(na.rm == TRUE){
    z = na.omit(x)
    c = (max(z)/min(z))
    return(c)
  }else{
    #x = x[!is.na(x)] flips directionality of logic tests
    #z = max(x)/min(x)
    if(any(is.na(x))== TRUE)
      return("NA")
  }
}
 maxminB.out <- apply(maxminB,1,maxmin1b, na.rm = TRUE)
 
## Brittni's code for Question 1B##
function(x, na.rm = FALSE) {
  if(na.rm == TRUE) {
    y <- na.omit(x)
    # Do max and min
    max <- max(y)
    min <- min(y)
    # Divide
    quot <- max/min
    return(quot)
  }else{ 
    if(any(is.na(x)) == TRUE) {
      return("You have NAs!")
    }else{
      max <- max(x)
      min <- min(x)
      # Divide
      quot <- max/min
      return(quot)
    }
  }
}
# Apply to matrix B over all rows 
quotsOfrowsB <- apply(B, 1, quot.NAs, na.rm = TRUE)


# Question 1C. THIS ONE WORKED
#remove 0s ==TRUE
#embedding if else statements to cover all four possibilities
maxmin2c <- function(x, na.rm = FALSE, zero.rm = FALSE){
if(na.rm == TRUE){
  if(zero.rm == TRUE){
    max(x[x>0], na.rm =TRUE)/min(x[x>0], na.rm=TRUE) # this line analogous to x[!is.na(x)]
  }else{
    max(x, na.rm==TRUE)/min(x,na.rm==TRUE)
  }
    }else{
    if(zero.rm==TRUE){
      max(x[x>0])/min(x[x>0])
    }else{
      max(x)/min(x)
    }
  }
}

ques1c <- apply(maxminC,1,maxmin2c, na.rm =TRUE)
ques1c



#1C. RUn function from B on MaxOverMinC.txt
maxminC <- read.table("MaxOverMinC.txt", header = FALSE)


#2 Custom function for x y coordinates
#good idea to put parentheses around each logic test

coord <- function(x){
  if(length(x)==2){ #for if you only have one pair of coordinates, or have x=0 and y=0
    #or use if(ncol(x)=2)
    slope = x[2]/x[1]
    b = x[2] - (slope * x[1])
    z = list(x=x, slope=slope, intercept = b)
    print(z)
  }else{
  slope = (x[4]-x[2])/(x[3]-x[1])
  b = x[3] - (slope * x[4]) 
  z = list(x=x,intercept=b, slope=slope)
  print(z)
}
}


pts <- read.table("points.txt", header = TRUE)
# need to rename variables so they match the function??
colnames(pts) <- c("x1", "y1", "x2", "y2") #get an error on this one


## for loop method to calculate m and b for exercise 3, ques 2, from Sakai##

slopeCalc1<-function(p1,p2=c(0,0)){
  # checking for x and y only provided for each point
  if((length(p1)!=2 | length(p2)!=2)){
    return("Error in provided coordinates")
  }else{
    m=(p1[2]-p2[2])/(p1[1]-p2[1])
    b=p1[2]-(m*p1[1])
    
    out=list(p1=p1,p2=p2,b=b,m=m)
    return(out)
  }
}

for(i in 1:nrow(pts)){
  x=slopeCalc1(p1=pts[i,1:2],p2=pts[i,3:4])
  print(x)
}


#stuff from class on 2.1.17
mean<-function(x){
mu=sum(x)/length(x)
return(list(length=length(x),mu=mu))
 }
y=c(1,2,3,4,5)
mean(y)
$length
$mu

y <- c(NA, 5,5,5,5,NA,6,5,NA)
mean <- function(x){
  if(any(is.na(x))){
    print("You have NAs")
  }else{
    print("you have no NAs")
  }
}

mean(y)
 

rm(list=ls())
w =12 # global env
f <- function(y){
  d = 8 #local env1
  h <- function(){
    return(d*(w+y))
  }
  return(h())
}
  environment(f)
  
  ls()
  is.str()
  f(2)
  }
}

## class 2.6.17 tree dbh

tree.dbh <- read.table("treeDBH.txt", header = TRUE)
#mean growth of each tree
#rate of growth from one year to next
#dim of tree.dbh is 200 x 15
#dim of output of growth increment is 200 x 14

#loop method for growth increments
inc <- matrix(NA, nrow(tree.dbh), ncol(tree.dbh)-1)
for(i in 1:nrow(inc)){
  for(j in 2:ncol(tree.dbh)){
    inc[i, (j-1)] = tree.dbh[i,j]-tree.dbh[i, j-1]
  }
}

#vector to fill with mean growth for each tree (row of inc)
meanGrowth <- numeric(nrow(inc))
for(i in 1:length(meanGrowth)){
  meanGrowth[i]=mean(inc[i,])
}

incGrowth <- function(x){
  inc = numeric(length(x)-1)
  for(i in 1:length(inc)){
    inc[i] = x[i+1]-x[i]
  }
  return(inc)
}

#apply incGrowth to dbh
inc = apply(tree.dbh, 1, incGrowth)
#apply mean to get mean growth rates of trees over 15 years
meanGrowth = apply(inc, 1, mean)
  }
}