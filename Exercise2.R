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

#or like this#
K = numeric(length=9)
for(i in 1:9){
  K[i] = sum(K[1:i])
}
# work inside out when debugging for loops, so set i = 1 or whatever

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

##another way using vectorized method
sumEvenvec = sum(eo[eo%%2==0])
sumEvenvec = sum(eo[eo%%2==1]) ## == 1 works same as != 0

##tapply works on 2 vectors: one that is numbers you want to do something with; and a second that is the membership
#or classification

sum.evens <- sapply(evens,sum)

#for loop method other group
evens = numeric(50)
odds = numeric(50)

for(i in 1:50){
  if(data$v1[,]%% 2 == 0)
    evens[i]=data$V1[i]
}else{
  odds[i] = data$v1[i]
  print(sum(odds, na.rm =T))
  print(sum(evens, na.r =T))
}

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

matrix.prac <- matrix(0, nrow = 10, ncol = 1) ## practicing matrix and looping
c = 0
for(i in 1:nrow(matrix.prac)){
  c = c + i*2
  matrix.prac[1,i] = c
}


