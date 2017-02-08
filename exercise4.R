##EXERCISE 4###

#Question 1: data labeled "T4" comes from "T3" experimental units
REU <- read.table("ThankYouREU.txt", header =TRUE, stringsAsFactors = FALSE)
dim(REU)
sub("T4", "T3", REU$treatment) #worked

Values <- REU$expression
Values

#now have to deal with treatment1, treat2

sub.treatments <- function(x){
     output = length(x)
  if(x = 't[a-zA-Z0-9]{5}')
     x = 'T1'
}

treatments.fixed <- sapply(treatments.only, FUN = sub.treatments)

#different approach
grep('t[a-zA-Z0-9]{3}', REU$treatment, value=TRUE)
#works, returns the treats and treatments
#really long way to do this but it worked; basically I split the original data into 2 vectors
#so I only had to deal with the treatments labels
REU.NEW <- sub("treat2", "T2", REU$treatment)
REU.NEW2 <- sub("treatment1","T1", REU.NEW)
#need to get all uppercase
uppercase <- chartr(old='t', new='T', REU.NEW2)

sub("treatment1", "T1", REU$treatment)
sub('T4', 'T3', REU$treatment)

#another for loop method , not working
treatments.only <- REU$treatment

convert <- length(treatments.only)
for(i in 1:length(convert)){
  if(i = T4){
     T3 = convert[i]
  }else{
    treatments.only[i] = convert[i]
  }
}


