# analyzing tree DBH data
# 1) estimate annual growth increments
# 2) calculate average growth of each tree
# 3) compare species means, sd, cv

# load data
dbh=read.table("treeDBH.txt",header=TRUE,row.names=1,sep="\t")

#####################
# loop-based method #
#####################

# matrix to fill with growth increments
inc=matrix(NA,nrow(dbh),ncol(dbh)-1)

# nested loop: i loops through trees (rows of inc) and j loops through years
for(i in 1:nrow(inc)){
	for(j in 2:ncol(dbh)){
		inc[i,(j-1)]=dbh[i,j]-dbh[i,(j-1)]
	}
}

# vector to fill with mean growth for each tree (row of inc)
meanGrowth=numeric(nrow(inc))
for(i in 1:length(meanGrowth)){
	meanGrowth[i]=mean(inc[i,])
}

# the data was organized in blocks by species so we can use simple
# indexing to calculate means and standard deviations of growth across 
# all individuals from that species
mean(meanGrowth[1:50])
mean(meanGrowth[51:100])
mean(meanGrowth[101:150])
mean(meanGrowth[151:200])

sd(meanGrowth[1:50])
sd(meanGrowth[51:100])
sd(meanGrowth[101:150])
sd(meanGrowth[151:200])

####################################
# super short, but not obvious way #
####################################

# we can remove the first column to make a "post" annual increment matrix and
# remove the last column to make a "pre" annual increment matrix and the annual
# growth increments will be the difference between those two matrices
inc=dbh[,-1]-dbh[,-ncol(dbh)]
# the average growths for individuals over time is the average of the rows
avgGrowth=rowMeans(inc)
# we can take the first letter of the tree ID (rownames) to tell us what species
# each row of data is
speciesCode=substring(rownames(dbh),first=1,last=1)

# tapply allows us to apply a function to elements of a vector that are identified
# by a classification or code (in this case speciesCode)
tapply(avgGrowth,speciesCode,mean)
tapply(avgGrowth,speciesCode,sd)
# to do coefficient of variation (standard deviatin/mean) we can define a simple function that tapply can use as an argument within tapply
tapply(avgGrowth,speciesCode,function(x) sd(x)/mean(x))

###########################
# a function+apply method #
###########################
# function that calculates incremental growth for a vector of dbhs
incGrowth<-function(x){
	inc=numeric((length(x)-1))
	for(i in 1:length(inc)){
		inc[i]=x[i+1]-x[i]
	}
	return(inc)
}

# apply incGrowth to dbh
inc=apply(dbh,1,incGrowth)
dim(inc)		# not that the apply function produces a 14 x 200 matrix (so our trees are now columns)

# apply mean to get mean growth rates of trees over 15 years
meanGrowth=apply(inc,2,mean)

# we can take the first letter of the tree ID (rownames) to tell us what species
# each row of data is
speciesCode=substring(rownames(dbh),first=1,last=1)

# tapply allows us to apply a function to elements of a vector that are identified
# by a classification or code (in this case speciesCode)
tapply(avgGrowth,speciesCode,mean)
tapply(avgGrowth,speciesCode,sd)
# to do coefficient of variation (standard deviatin/mean) we can define a simple function that tapply can use as an argument within tapply
tapply(avgGrowth,speciesCode,function(x) sd(x)/mean(x))




#********************************************************
# simulating tree growth data that can then be summarized

# specifying number of species and individuals per species
individualsPerSpecies=50
Nspecies=4

# average growth rate for the 4 species (I just made these up)
meanGrowthRates=c(1,3,7,9)

# how many years of growth to simulate
yearsofGrowth=15

# initial size and variation in that (used for random draw)
initialSize=3	# mm
initialSizeSD=0.25	# mm

# matrix to hold simulated dbh data
treeDBH=matrix(NA,Nspecies*individualsPerSpecies,yearsofGrowth)

# loop through species and number of individuals per species in "block-type" nested loop
for(i in 1:Nspecies){
	for(j in 1:individualsPerSpecies){
		# recall we did this in class, but this formulation with (i-1)*block size + index within block allows you to loop through blocks of the same size and fill in a matrix
		# The tree dbh simulation is of the form year*annual_growth_rate+initial_size+observation_error
		# annual_growth_rate, initial_size, and observation_error are all randomized (determined by a draw from a random distribution)
		treeDBH[((i-1)*individualsPerSpecies+j),]=(1:yearsofGrowth)*rnorm(1,meanGrowthRates[i],meanGrowthRates[i]/5)+rnorm(1,initialSize,initialSizeSD)+rnorm(yearsofGrowth,0,sd=3)
	}
}

# add treeIDs as row names and year numbers as column names
rownames(treeDBH)=paste(rep(c('aspen','walnut','maple','oak'),each=individualsPerSpecies),rep(1:individualsPerSpecies,Nspecies),sep="")
colnames(treeDBH)=paste("year",1:yearsofGrowth,sep="")

# write the matrix to a tab-delimited text file
write.table(treeDBH,"treeDBH.txt",sep="\t")