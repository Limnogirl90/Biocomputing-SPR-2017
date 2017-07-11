# Question 1
# calculate the diameter of each planet in kilometers

# miles to km conversion factor
mi2km=0.621

# planet circumferences in miles
C=c(9525,23628,24900,13264,279118,235299,99786,96692)
names(C)=c("mercury","venus","earth","mars","jupiter","saturn","uranus","neptune")

# convert circumferences to km
Ckm=C/mi2km

# calculate radius from circumference
r=Ckm/(2*pi)

# diameter from radius
d=r*2

# Question 2
# create a 5x7 matrix containing only 7's
B=matrix(7,nrow=5,ncol=7)
B=matrix(rep(7,5*7),nrow=5,ncol=7)
(B=matrix(c(7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),nrow=5,ncol=7))
B=rbind(rep(7,7),rep(7,7),rep(7,7),rep(7,7),rep(7,7))

# Question 3
# summarizing matrix M
M=read.table("exercise1prob3.txt",sep="\t")

# a
M
# b
mean(M[,2],na.rm=TRUE)
# c
M=M[order(M[,1],decreasing=TRUE),]
# d
tf=M[,2]==min(M[,2],na.rm=TRUE)
tf[is.na(tf)]=FALSE
M[tf,1]

which(M[,2]==min(M[,2],na.rm=TRUE))
M[which(M[,2]==min(M[,2],na.rm=TRUE)),]

# e
subM=M[M[,1]>2000,]
median(subM[,2],na.rm=TRUE)

# Question 4
# group data
groupData=list(ages=c(35,33,6,4),names=c("Stuart","Manda","Finn","Soren"),MvF=c("male","female","male","male"))

# a
mean(2*(groupData$ages[groupData$MvF=="female"]))
# b
groupData$names[groupData$MvF=="male"]
# c
lastName=sort(groupData$names)[length(groupData$names)]
groupData$ages[groupData$names==lastName]


# Question 5
# water chem data
wc=read.table("water_chem.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
dim(wc)
head(wc)

# coefficient of variation
wcMeans=colMeans(wc[,3:7])
wcSDs=apply(wc[,3:7],2,sd)
CoVs=wcSDs/wcMeans

# difference between north and south means
colnames(wc)
northMeanTP=mean(wc$TP[wc$north=="north"])
southMeanTP=mean(wc$TP[wc$north=="south"])
northMeanTP-southMeanTP