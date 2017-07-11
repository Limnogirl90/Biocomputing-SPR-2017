##EXERCISE 4###

#Question 1: data labeled "T4" comes from "T3" experimental units
REU <- read.table("ThankYouREU.txt", header =TRUE, stringsAsFactors = FALSE)
dim(REU)

##make all treatments just one letter##
#get rid of all letters in the middle and make sure have number at end#
#parentheses around items in a string mean they are groups to which I can back-reference later with \\
#[tT] matches upper or lower case letter, either or

REU$treatment <- sub("[tT][a-zA-Z]*([0-9])", "T\\1", REU$treatment)
REU$treatment <- sub("T4", "T3", REU$treatment)
##could make everything uppercase or lowercase first that way don't have to deal with that
#function unique() use it next time
unique(REU$treatment)
#could also use grep in [] to force treatments to T1,2,3

###QUESTION 2###
d <- read.table("dna.txt", stringsAsFactors = FALSE)
b = read.table("dna.txt", stringsAsFactors = FALSE)
r = gsub("ATG", "atg", b)
r = gregexpr("(atg){1}[A-Z]*((TAG)|(TAA)|(TGA)){1}", r) #because preceeding element will be
#matched 0 or more times
regmatches(b,r)
r

setwd("C:\\Users\\klbai\\Desktop\\Biocomputing SPR 2017")
## another way from class 2/13/17###
dna = DNA[1,1]
x = gregexpr("ATG",dna)
y = gregexpr("TAG|AAG|TGA", dna)
start = unlist(x) ##had to unlist to get the locations
stop = unlist(y)
length = stop-start
genes = data.frame(start,stop,length)



##QUESTION 3 FIXING FILE NAME
#load in data
water <- read.table("IndianaCountyPercentWater.txt", header = TRUE, stringsAsFactors = FALSE)
pop <- read.table("IndianaCountyPopDensity.txt", header = TRUE, stringsAsFactors = FALSE)

#merge ended up working, but I needed to do some tinkering to make it look good
 merge.pop <- merge(pop, water, by.x = 'countyName', all.x = TRUE) # this gets the county names consistent but
 #need percent water
 just.water <- water$percentWater #I split the percent file so I would have a vector of just percent water values
 just.water
 combined.data <- cbind(merge.pop,just.water)
 combined.data$percentWater<- NULL # for some reason the naming of the columns was weird, so I ommitted one
 #rename just.water
 colnames(combined.data)[3] <- "percentWater" #and renamed the column with percentwater
 
 ## ANOTHER WAY ##
 pop$countyName <- tolower(pop$countyName)
 pop$countyName <- gsub(" county", "", pop$countyName)
 water$countyName = paste(water$countyName, "", sep="") # above line negates the need for this line
 merge(pop, water)
 
 ##another way Jones##
 pop = pop[order(pop$countyName),]
 water = water[order(water$countyName)]
 dim(pop)
 dim(water)
 sum(pop$countyName==water$countyName) ##trusting yourself that the data are in the correct order
 
 ##Question 4: Generate a script to provide min daily, max daily, and total precip for each city##
 #import data
e = unzip("PrecipData.zip")
Cityname <- gsub("./PrecipData/","",e, fixed =T) #get rid of the /PrecipData/
Cityname.1 <- gsub("Precip.txt", "", Cityname, fixed =T)
table = data.frame(Cityname.1, minimum_daily = numeric(length(e)), maximum_daily = numeric(length(e)), sum_daily =numeric(length(e)))


for(i in 1:length(e)){
  table[i,4] = sum(scan(e[i]))
  table[i,2] = min(scan(e[i]))
  table[i,3] = max(scan(e[i]))
}
table

##ANOTHER WAY## 
#precipfiles are our set, # precip files = # rows, columns dictated by types of info we want to fill in H76
files <- unlist(files)
out <- matrix(NA, nrow=18, ncol=4)
colnames(out) = c("City", "Min", "Max", "Total")
for(i in 1:length(files)){
  data = read.table("file[i]", stringsAsFactors = FALSE)
  name <- files[i]
  out[i,1] = gsub("Precip.txt", "", name)
  out[i,2] = min(data)
  out[i,3] = max(data)
  out[i,4] = sum(data)
}
as.numeric(summary[,4]/4)
#for loop doesn't create its own environment, the variables are in the global environment