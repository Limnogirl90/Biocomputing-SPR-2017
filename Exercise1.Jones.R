> rep(7)
[1] 7
> matrix(1:7,5,7)
[,1] [,2] [,3] [,4] [,5]
[1,]    1    6    4    2    7
[2,]    2    7    5    3    1
[3,]    3    1    6    4    2
[4,]    4    2    7    5    3
[5,]    5    3    1    6    4
[,6] [,7]
[1,]    5    3
[2,]    6    4
[3,]    7    5
[4,]    1    6
[5,]    2    7
> matrix(1:7,5,7,byrow=TRUE)
[,1] [,2] [,3] [,4] [,5] [,6]
[1,]    1    2    3    4    5    6
[2,]    1    2    3    4    5    6
[3,]    1    2    3    4    5    6
[4,]    1    2    3    4    5    6
[5,]    1    2    3    4    5    6
[,7]
[1,]    7
[2,]    7
[3,]    7
[4,]    7
[5,]    7
> rep(7)
[1] 7
> matrix(7,36,nrow=5,ncol=7)
[,1] [,2] [,3] [,4] [,5] [,6]
[1,]    7    7    7    7    7    7
[2,]    7    7    7    7    7    7
[3,]    7    7    7    7    7    7
[4,]    7    7    7    7    7    7
[5,]    7    7    7    7    7    7
[,7]
[1,]    7
[2,]    7
[3,]    7
[4,]    7
[5,]    7
> matrix(7,36,5,7)
[,1] [,2] [,3] [,4] [,5]
[1,]    7    7    7    7    7
[2,]    7    7    7    7    7
[3,]    7    7    7    7    7
[4,]    7    7    7    7    7
[5,]    7    7    7    7    7
[6,]    7    7    7    7    7
[7,]    7    7    7    7    7
[8,]    7    7    7    7    7
[9,]    7    7    7    7    7
[10,]    7    7    7    7    7
[11,]    7    7    7    7    7
[12,]    7    7    7    7    7
[13,]    7    7    7    7    7
[14,]    7    7    7    7    7
[15,]    7    7    7    7    7
[16,]    7    7    7    7    7
[17,]    7    7    7    7    7
[18,]    7    7    7    7    7
[19,]    7    7    7    7    7
[20,]    7    7    7    7    7
[21,]    7    7    7    7    7
[22,]    7    7    7    7    7
[23,]    7    7    7    7    7
[24,]    7    7    7    7    7
[25,]    7    7    7    7    7
[26,]    7    7    7    7    7
[27,]    7    7    7    7    7
[28,]    7    7    7    7    7
[29,]    7    7    7    7    7
[30,]    7    7    7    7    7
[31,]    7    7    7    7    7
[32,]    7    7    7    7    7
[33,]    7    7    7    7    7
[34,]    7    7    7    7    7
[35,]    7    7    7    7    7
[36,]    7    7    7    7    7
> matrix(rep(7,36),5,7)
[,1] [,2] [,3] [,4] [,5] [,6]
[1,]    7    7    7    7    7    7
[2,]    7    7    7    7    7    7
[3,]    7    7    7    7    7    7
[4,]    7    7    7    7    7    7
[5,]    7    7    7    7    7    7
[,7]
[1,]    7
[2,]    7
[3,]    7
[4,]    7
[5,]    7
Warning message:
  In matrix(rep(7, 36), 5, 7) :
  data length [36] is not a sub-multiple or multiple of the number of rows [5]
> matrix(rep(7,70),5,7)
[,1] [,2] [,3] [,4] [,5] [,6]
[1,]    7    7    7    7    7    7
[2,]    7    7    7    7    7    7
[3,]    7    7    7    7    7    7
[4,]    7    7    7    7    7    7
[5,]    7    7    7    7    7    7
[,7]
[1,]    7
[2,]    7
[3,]    7
[4,]    7
[5,]    7
> M=read.table("exercise1prob3.txt",sep="\t")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'exercise1prob3.txt': No such file or directory
> setwd("/Users/stuartjones/Documents/Teaching/AdBiostats/AdBiostats2017/Module01-intro2R/Exercise01")
> rownames(M)
Error in rownames(M) : object 'M' not found
> M=read.table("exercise1prob3.txt",sep="\t")
> M
V1   V2
1 1998 10.0
2 1999 12.5
3 2000 15.6
4 2001 19.5
5 2002 24.4
6 2003   NA
7 2004 36.2
8 2005 27.7
> rev(M[,1])
[1] 2005 2004 2003 2002 2001 2000 1999
[8] 1998
> revM=M[rev(M[,1]),]
> M
V1   V2
1 1998 10.0
2 1999 12.5
3 2000 15.6
4 2001 19.5
5 2002 24.4
6 2003   NA
7 2004 36.2
8 2005 27.7
> revM
V1 V2
NA   NA NA
NA.1 NA NA
NA.2 NA NA
NA.3 NA NA
NA.4 NA NA
NA.5 NA NA
NA.6 NA NA
NA.7 NA NA
> M
V1   V2
1 1998 10.0
2 1999 12.5
3 2000 15.6
4 2001 19.5
5 2002 24.4
6 2003   NA
7 2004 36.2
8 2005 27.7
> order(M[,1])
[1] 1 2 3 4 5 6 7 8
> order(M[,1],decreasing=TRUE)
[1] 8 7 6 5 4 3 2 1
> revM=M[order(M[,1],decreasing=TRUE),]
> revM
V1   V2
8 2005 27.7
7 2004 36.2
6 2003   NA
5 2002 24.4
4 2001 19.5
3 2000 15.6
2 1999 12.5
1 1998 10.0
> nrow(M):1
[1] 8 7 6 5 4 3 2 1
> order(M[,1])
[1] 1 2 3 4 5 6 7 8
> order(-M[,1])
[1] 8 7 6 5 4 3 2 1
> -M[,1]
[1] -1998 -1999 -2000 -2001 -2002
[6] -2003 -2004 -2005
> tf=M[,2]==min(M[,2],na.rm=TRUE)
> tf
[1]  TRUE FALSE FALSE FALSE FALSE
[6]    NA FALSE FALSE
> tf[is.na(tf)]=FALSE
> tf
[1]  TRUE FALSE FALSE FALSE FALSE
[6] FALSE FALSE FALSE
> M[tf,1]
[1] 1998
> apply(M,2,min,na.rm=TRUE)
V1   V2 
1998   10 
> ?apply
> apply(revM,2,min,na.rm=TRUE)
V1   V2 
1998   10 
> M
V1   V2
1 1998 10.0
2 1999 12.5
3 2000 15.6
4 2001 19.5
5 2002 24.4
6 2003   NA
7 2004 36.2
8 2005 27.7
> M[1,2]=1e6
> M
V1       V2
1 1998 1.00e+06
2 1999 1.25e+01
3 2000 1.56e+01
4 2001 1.95e+01
5 2002 2.44e+01
6 2003       NA
7 2004 3.62e+01
8 2005 2.77e+01
> apply(M,2,min,na.rm=TRUE)
V1     V2 
1998.0   12.5 
> groupData=list(ages=c(35,33,6,4),names=c("Stuart","Manda","Finn","Soren"),MvF=c("male","female","male","male"))
> 
  > # a
  > mean(2*(groupData$ages[groupData$MvF=="female"]))
[1] 66
> # b
  > groupData$names[groupData$MvF=="male"]
[1] "Stuart" "Finn"   "Soren" 
> groupData$MvF=="male"
[1]  TRUE FALSE  TRUE  TRUE
> which(groupData$MvF=="male")
[1] 1 3 4
> wc=read.table("water_chem.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
> dim(wc)
[1] 8 7
> head(wc)
Lake northVsouth area_ha TP_mgm3 DOC_gm3     R
1 allequash       south   168.4    39.8     3.7   9.8
2    muskie       south   396.3     9.4     4.5   7.2
3     brown       north    32.9    55.9     9.1   5.8
4  crampton       north    25.8    13.2     4.0  12.4
5   crystal       south    36.7     4.4     1.6   3.5
6  kickapoo       north     7.9    34.9    14.2 119.8
GPP
1 10.4
2  4.4
3  4.1
4 10.6
5  4.6
6 75.3
> wcMeans=colMeans(wc[,3:7])
> wcSDs=apply(wc[,3:7],2,sd)
> CoVs=wcSDs/wcMeans
>planet.stuff <- c(10000,200000,3456,23456,18764,1988)
>planet.km <- sapply(planet.stuff, (planet.stuff/.621))
