## Exercise 2 Biostatistics
# Question 1: Create a vector containing numbers 1-9, calculate cumulative sums for vector

## this works!!
length(v1sum)
q1 <- numeric(length = 9)
c = 0
for(i in 1:9){
  c = c + i
  q1[i] = c
}


##Question 2: calculate sums of all even and all odd numbers with a loop and an "apply" family function
#use which to separate the evens and odds into 2 vectors

even <- which(evenodd$V1 %% 2 == 0) # position of each even number
odd <- which(evenodd$V1 %% 2 != 0) # position of each odd number
even1 <- c(evenodd[which(evenodd$V1 %% 2 == 0),]) # vector of the even values
odd1 <- c(evenodd[which(evenodd$V1 %% 2 != 0),]) # vector of odd values
#evens <- list(even1)
#odds <- list(odd1)
combined <- list(even = even1, odd = odd1) # can combine even and odd sums in one list
combinedsums <- sapply(combined, sum)

sum.evens <- sapply(evens,sum)

#for loop method

evensum = 0             ## 2 for loops for even and odd sums, it worked, want to make into one for loop
for(i in even1){
  evensum = evensum + i
}

oddsum = 0
for(i in odd1){
  oddsum = oddsum + i
}
  {else(NA)

  }

evenodd.vector2 %% 2 == 0 # TF statement
evenodd.vector2[evenodd.vector2 %% 2 == 0] ## this worked!

lapply(alist, function(x) if (x > 7) {1} else {0}) ## found on stack exchange for help
lapply(evenodd.vector2, function(x) if (x %% 2 == 0) {cumsum(x)}) #worked but gave cumsum for all numbers
lapply(evenodd.vector2, function(x) if(x %% 2 == 0) {cumsum(x)} else(x %% 2 != 0) {NA})

## Question 3: create a vector that is 10 elements long and contains 1s or 0s
x <- c(0:1)
vector2 <- sample(c(0,1), 10, replace = TRUE) ## this worked!

#convert all 1's to TRUE and 0's to FALSE

#method 1:
y <- ifelse(test = vector2>0, yes = TRUE, no = FALSE) # this worked!

#method 2: using just a simple logic statement
false = vector2 == 1

#method 3:
v2 <- sample(c(0,1), 10, replace = TRUE)
for (i in v2){
  if(i == 1){
    print(TRUE)
  } else {
    print(FALSE)
  }
  }

#method 4
v2 <- sample(c(0,1), 10, replace = TRUE)
for (i in v2){
  if(i == 1){
    print(TRUE)
  } else if(i == 0){
    print(FALSE)
  }
}
    ##Question 4: create a batch of code that reports index for the element at the beginning of
    ##runs and repeated values in a vector

ques4 <- sample(1:50, 50, replace = TRUE) #duplicate function
diffs <- ques4[-1L] != ques4[-length(ques4)]
idx <- c(which(diffs), length(ques4))

runs <- duplicated(ques4) #??

#or

q4 <- c(1,2,3,4,5,6,6,6,6,67,7,7,77,77,3,3,3,3,3,3,4,5,6,7,7,7,7)
rle.q4 <- rle(q4)

#Question 5: Create a matrix, 5 replicates and 5 treatments
# need to create a matrix, 25 rows and 3 columns

nutrient <- matrix(0, nrow = 25, ncol = 3)
nutrient ## empty matrix of correct dimensions
#create a loop to make treatment column (for i in nrow)

# just an example from stack exchange
x=matrix(data=NA, nrow=n, ncol=k)
for(j in 1:k){
  for(i in 1:n){
    x[i,j] = rnorm(1, mu, sigma)
  }
}

# me replicating the example matrix loop above:
> ques5 <- matrix(c(1,2,3,4,5), nrow = 5, ncol = 1, byrow = FALSE)
> 1:nrow(ques5)
[1] 1 2 3 4 5
> ques5[5,]
[1] 5
> rows <- rep(1:nrow(ques5), ques5[5,])
replicates <- rep(c(1,2,3,4,5), each = 5) ## made treatment levels
cbind(replicates, rows) #this created 2 columns, treatment and replicates, works!

for(i in 1:25){
    nutrient[i] = rep(1:5, each = 5)
}
## Exercise 2 Biostatistics
# Question 1: Create a vector containing numbers 1-9, calculate cumulative sums for vector

## this works!!
length(v1sum)
q1 <- numeric(length = 9)
c = 0
for(i in 1:9){
  c = c + i
  q1[i] = c
}


##Question 2: calculate sums of all even and all odd numbers with a loop and an "apply" family function
#use which to separate the evens and odds into 2 vectors

even <- which(evenodd$V1 %% 2 == 0) # position of each even number
odd <- which(evenodd$V1 %% 2 != 0) # position of each odd number
even1 <- c(evenodd[which(evenodd$V1 %% 2 == 0),]) # vector of the even values
odd1 <- c(evenodd[which(evenodd$V1 %% 2 != 0),]) # vector of odd values
#evens <- list(even1)
#odds <- list(odd1)
combined <- list(even = even1, odd = odd1) # can combine even and odd sums in one list
combinedsums <- sapply(combined, sum)

sum.evens <- sapply(evens,sum)

#for loop method

evensum = 0             ## 2 for loops for even and odd sums, it worked, want to make into one for loop
for(i in even1){
  evensum = evensum + i
}

oddsum = 0
for(i in odd1){
  oddsum = oddsum + i
}
  {else(NA)

  }

evenodd.vector2 %% 2 == 0 # TF statement
evenodd.vector2[evenodd.vector2 %% 2 == 0] ## this worked!

lapply(alist, function(x) if (x > 7) {1} else {0}) ## found on stack exchange for help
lapply(evenodd.vector2, function(x) if (x %% 2 == 0) {cumsum(x)}) #worked but gave cumsum for all numbers
lapply(evenodd.vector2, function(x) if(x %% 2 == 0) {cumsum(x)} else(x %% 2 != 0) {NA})

## Question 3: create a vector that is 10 elements long and contains 1s or 0s
x <- c(0:1)
vector2 <- sample(c(0,1), 10, replace = TRUE) ## this worked!

#convert all 1's to TRUE and 0's to FALSE

#method 1:
y <- ifelse(test = vector2>0, yes = TRUE, no = FALSE) # this worked!

#method 2: using just a simple logic statement
false = vector2 == 1

#method 3:
v2 <- sample(c(0,1), 10, replace = TRUE)
for (i in v2){
  if(i == 1){
    print(TRUE)
  } else {
    print(FALSE)
  }
  }

#method 4
v2 <- sample(c(0,1), 10, replace = TRUE)
for (i in v2){
  if(i == 1){
    print(TRUE)
  } else if(i == 0){
    print(FALSE)
  }
}
    ##Question 4: create a batch of code that reports index for the element at the beginning of
    ##runs and repeated values in a vector


#attempt 1
q4.list <- as.factor(q4)
q4.list
q4.again <- which(duplicated(q4))
q4[q4.again]
#attempt 2
unique.q4 <- !duplicated(q4)
seq_along(q4)[unique.q4] #the indexes
q4[unique.q4] #the values  # close but only gave unique values, not necessarily the runs of repeats

# another way using rle2
q4.rle2 <- rle2(q4, indices = TRUE, return.list = TRUE)
mapply(function(l,v){ ifelse(l==1,NA,v) }, rle2(q4)$lengths, rle2(q4)$values)

### This for loop method worked ###

findem = function(argument){ #make a function and argument is place holder for q4
  replicates = c() # preallocating empty vector to store runs in
  for(i in 2:length(argument)){# started with 2nd element because index of 1st minus one doesnt make sense
    if(argument[i]==argument[i-1]) # for every element that has same value, append them to replicates vector
    replicates <- append(replicates,i-1)
  }
  #print(replicates)
  replicates2 = c()
  for(i in 1:length(replicates)){
    #print("i: ")
    #print(i)
    if(i==1){ #this is first start of the run that can't be compared to previous run
      replicates2 = c(replicates[i])
      #print("this is the first run")
    }
    else {
      if(replicates[i-1] == replicates[i]-1){ ## checking within same run
        #print( "this is the same run")
      }
      else{
        replicates2 = c(replicates2,replicates[i]) #new run
        #print("i found a new run")
      }
    }
  }
  replicates2
}

#going to input indexes into vector now
q4.index <- findem(q4) # preallocating new empty vector to store indexes of the beginning of runs
q4.times <- c() ## preallocating empty vector to store the number of times an element is repeated
for(i in 1:length(q4.index)){
  first = q4.index[i]
      j = q4.index[i]
  count = 0
  while(j <= length(q4) & q4[j]==q4[first]){ # used a while loop because I want to count within runs
    j = j + 1
    count = count + 1
  }
  q4.times = append(q4.times,count)
}


#Question 5: Create a matrix, 5 replicates and 5 treatments
# need to create a matrix, 25 rows and 3 columns

nutrient <- matrix(0, nrow = 25, ncol = 3)
nutrient ## empty matrix of correct dimensions
#create a loop to make treatment column (for i in nrow)

# just an example from stack exchange
nutrient <- c()
for(i in 1:25){ ## this gave me a 25*25 matrix of the sequence 1 2 3 4 5 on all columns
  nutrient <- cbind(nutrient, rep(1:5, 5))
}

# trying a method using vectors
treatments <- rep(c(1,2,3,4,5), each = 5)
reps <- rep(c(1:5),5)
nutrient <- matrix(0, nrow = 25, ncol = 2)

for(i in 1:nrow(nutrient)){
  nutrient[i,1] = treatments[i]
}
for(j in 1:nrow(nutrient)){
  nutrient[j,2] = reps[j]
}


##non-loop method
ques5 <- matrix(c(1,2,3,4,5), nrow = 5, ncol = 1, byrow = FALSE)
1:nrow(ques5)
ques5[5,]
rows <- rep(1:nrow(ques5), ques5[5,])
replicates <- rep(c(1,2,3,4,5), each = 5) ## made treatment levels
cbind(replicates, rows) #this created 2 columns, treatment and replicates, works!

## Class 1.30.17 going over for loops and subsetting##
v = c(4,3,1,17,20) # c for concatenate
length(v)
out = numeric(length(v))
for(i in 1:length(v)){ ##need to use 1:length() because you need to index for the empty preallocated vector
out[i] = v[i]/2
}
