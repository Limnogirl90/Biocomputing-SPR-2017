##Question 1
#Vector is one dimensional, equivalent to a row of data in excel, and can only hold one type of data mode.
#Matrix is 2 dimensional and like a vector can only hold one data mode

#Dataframe can hold multiple data modes
#List also holds multiple data modes BUT??

#if-else can be used in a function and is used to deal with typically dichotomous situations?
#for loop runs through each element in a vector or matrix, can embed if-else in a for loop

##Question 2##
#write the code necessary to make a 4x4 matrix of 1-12

exam.matrix <- matrix(1:12, nrow = 3, ncol = 4, byrow = T)
exam.matrix

##write the code to access only column 3##

exam.matrix[,3]

#what code would return 7?##

exam.matrix[2,3]

##QUESTION 3###
words = c("cat","hat","fish","dish","fox","socks")
#capitalize all vowels
vowels <- sub("[a|i|o]", "$", words)

vowels.upper <- chartr(old="aoi", new = "AOI", words) ##YES

#what code would only return cat and hat?##

grep("at", words, value=T) ##YES

##QUESTION 4###
##create a 100 x25 matrix called D
D <- matrix(sample(c(1:100), size = 2500, replace = TRUE, prob = NULL), 100, 25)
#find range for each row
 range.D <- matrix(NA, nrow(D), ncol = 1)
 colnames(range.D)<- ("Range")
 for(i in 1:nrow(range.D)){
   range.D[i,] = max(D[i,]) - min(D[i,])
 }
 
##Now write a custom function and use apply family function to apply to D##
 
range.function <- function(x){
    range=matrix(NA, nrow(D), ncol=1)
    for(i in 1:nrow(range)){
      range[i,]=max(D[i,]) - min(D[i,])
    }
    return(range)
  }

range <- apply(D,1, range.function)

##trying apply function again##
range.function <- function(x, na.rm=F, zero.rm =F){
  if(na.rm == TRUE){
    max(x, na.rm ==TRUE)-min(x, na.rm==TRUE)
  }else{
    max(x)-min(x)
  }
}

range <- apply(D,1,range.function)
##custom function##

CV.function <- function(x, na.rm=F, zero.rm =F){
  if(na.rm == TRUE){
   sd(x, na.rm ==TRUE)/mean(x, na.rm ==TRUE)
  }else{
    sd(x)/mean(x)
  }
}

CV.D <- apply(D,1,custom.function)

##modify loop to return range for rows that have max greater than 25 and return NA for cases 
#where MAX is less than 25

range.D <- matrix(NA, nrow(D), ncol = 1)

for(i in 1:nrow(range.D)){
  if(max(D[i,]) > 25){
  range.D[i,] = max(D[i,]) - min(D[i,])
  }else{
  range.D[i,] = NA
  }
}

#modified for loop for min <25

range.D <- matrix(NA, nrow(D), ncol = 1)

for(i in 1:nrow(range.D)){
  if(min(D[i,]) < 25){
    range.D[i,] = max(D[i,]) - min(D[i,])
  }else{
    range.D[i,] = NA
  }
}
##Question 5##


M=matrix(NA,10,10)
for(i in 1:10){
  for(j in 1:10){
    M[i,j]=i*j
  }
}

