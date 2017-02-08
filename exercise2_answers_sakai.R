## EXERCISE 2 ANSWERS FROM SAKAI##
rm(list=ls())
setwd("~/Documents/Teaching/AdBiostats/AdBiostats2017/Module01-intro2R/Exercise02")

##### BIOS 60751
##### Exercise #2

# Question 1
# calculate the cumulative sums for a vector from 1 to 9
# create vector
v=1:9

# create vector to hold cumulative sums
csums=numeric(length(v))

# use loop to calculate
for(i in 1:length(v)){
  csums[i]=sum(v[1:i])
}

csums

cumsum(v)

# Question 2
# calculate the sum of all even numbers and all odd numbers in the vector in "evensVSodds.txt"
#load file
eo=read.table("evensVSodds.txt",header=FALSE,sep="\t")
eo=unlist(eo)
# LOOP METHOD
sumEven=0
sumOdd=0
for(i in eo){
  if(i%%2==1){
    sumOdd=sumOdd+i
  }else{
    sumEven=sumEven+i
  }
}

sumEven
sumOdd

# VECTORIZED METHOD
sumEvenVec=sum(eo[eo%%2==0])
sumOddVec=sum(eo[eo%%2==1])

# using apply function
EvO=rep("even",length(eo))
EvO[eo%%2==1]="odd"
tapply(eo,EvO,sum)

# Question 3
# Create a vector of 1s and 0s and then replace 1s with TRUEs and 0s with FALSEs
# create vector using sample()
v=sample(c(0,1),10,replace=TRUE)

# LOOP METHOD
vLoop=vector(length=length(v))
for(i in 1:length(vLoop)){
  if(v[i]==1){
    vLoop[i]=TRUE
  }else{
    vLoop[i]=FALSE
  }
}

# ifelse() method
vIfElse=ifelse(v==1,TRUE,FALSE)

# boolean method
vBoolean=(v==1)


# Question 4
# Report starting element and length of runs in "findRuns.txt"
# load data from text file
findRuns=read.table("findRuns.txt",header=FALSE,sep="\t")
# convert data from a single-column dataframe to a vector
findRuns=unlist(findRuns)

# create a variable out that is currently undefined
out=NULL
# I will use this variable cur to hold onto the previous number in the vector;
# this is analagous to using findRuns[i-1]
cur=findRuns[1]
# this is a counter that I use to keep track of how long a run of repeated values is;
# if there are not repeated values than this count equals 1
count=1

# loop through each entry of our vector (except the 1st one, which we set to cur above)
for(i in 2:length(findRuns)){
  # test if the ith value in the vector findRuns equals the previous (stored in cur)
  if(findRuns[i]==cur){
    # test whether count is 1 (we aren't in the middle of a run) or >1 (in the middle of a run)
    if(count==1){
      # if the ith value in the vector equals the previous (stored in cur) and count is 1, we
      # are at the beginning of a run and we want to store this value (we temporarily store it in 'start')
      start=(i-1)
    }
    # we add one to count because the run continued based on the ith value of findRuns being equal to
    # the previous (stored in cur)
    count=count+1
    # if the ith value in findRuns is not the same as the previous (stored in cur) we either are not in a run
    # or we are ending a run
  }else{
    # if count is greater than 1 it means we were in a run and must be exiting one
    if(count>1){
      # add a row to 'out' that will hold the starting positions in the first column and the length
      # of runs in the second column; this appends rows to out after finding and counting each run
      out=rbind(out,c(start,count))
      # reset count to 1 because we just exited a run
      count=1
    }
  }
  # remember cur holds the previous element in findRuns, so we need to update this after each time
  # we go through the for loop
  cur=findRuns[i]
}

# give out column names and print it
colnames(out)=c('start','length')
out


# Because the examples in lecture used i and i-1 for comparisons here is a version of the above code
# where I replaced the use of "cur" with findRuns[i-1]


# create a variable out that is currently undefined
out=NULL

# this is a counter that I use to keep track of how long a run of repeated values is;
# if there are not repeated values than this count equals 1
count=1

# loop through each entry of our vector (except the 1st one, which we set to cur above)
for(i in 2:length(findRuns)){
  # test if the ith value in the vector findRuns equals the previous (i-1)
  if(findRuns[i]==findRuns[i-1]){
    # test whether count is 1 (we aren't in the middle of a run) or >1 (in the middle of a run)
    if(count==1){
      # if the ith value in the vector equals the previous (stored in cur) and count is 1, we
      # are at the beginning of a run and we want to store this value (we temporarily store it in 'start')
      start=(i-1)
    }
    # we add one to count because the run continued based on the ith value of findRuns being equal to
    # the previous (stored in cur)
    count=count+1
    # if the ith value in findRuns is not the same as the previous (stored in cur) we either are not in a run
    # or we are ending a run
  }else{
    # if count is greater than 1 it means we were in a run and must be exiting one
    if(count>1){
      # add a row to 'out' that will hold the starting positions in the first column and the length
      # of runs in the second column; this appends rows to out after finding and counting each run
      out=rbind(out,c(start,count))
      # reset count to 1 because we just exited a run
      count=1
    }
  }
}

# give out column names and print it
colnames(out)=c('start','length')
out


# Question 5
# Create design matrix - this highlights how one can use nested loops to do something in 
# repetative blocks that are the same size (in this case 5 replicates within 5 treatments)

# the trick is that you use one index variable to increment "inside" of a block, and the outer index variable
# to dictate which block you are in
# this will always take the form (i-1)*B+j, where i is the outer index variable and can loop from 1 to the number of blocks you have
# B is the number of elements within a block and j is the inner index variable that will loop from 1 to B

# LOOP METHOD
# preallocate a matrix that is 5 treatment levels * 5 replicates long as rows and 2 columns to hold the treatment number and replicate number
designLoop=matrix(NA,5*5,2)

# nested for loops in the form described above
# outer loop that loops across our "blocks" (these are treatment levels in our case)
for(i in 1:5){
  # inner loop that loops through elements within each "block" (these are replicates in our case)
  for(j in 1:5){
    # here is where we use the little trick piece of indexing; it works because when i=1, (i-1)*B will be zero and then each subsequent
    # loop through the outer loop (using i as the index variable) allows us to index the 2nd, 3rd, etc. block and again j indexes within these blocks
    designLoop[(i-1)*5+j,1]=i
    designLoop[(i-1)*5+j,2]=j
  }
}

# NO LOOP METHOD -- this is a lot easier, but doesn't use loops
designNoLoop=cbind(sort(rep(1:5,5),decreasing=FALSE),rep(1:5,5))



