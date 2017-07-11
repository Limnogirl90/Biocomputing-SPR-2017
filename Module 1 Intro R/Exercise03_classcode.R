##### BIOS 60751
##### Exercise #3
rm(list=ls())


# Question 1
# Function development is a process

# A. Basic function
MaxOverMin<-function(x){
	max(x)/min(x)
}

a=read.table("MaxOverMinA.txt",header=FALSE)
apply(a,1,MaxOverMin)


# B. Dealing with NAs
b=read.table("MaxOverMinB.txt",header=FALSE)
apply(b,1,MaxOverMin)

MaxOverMinNA<-function(x,na.rm=FALSE){
	if(na.rm==TRUE){
		max(x,na.rm=TRUE)/min(x,na.rm=TRUE)
	}else{
		max(x)/min(x)
	}
}

apply(b,1,MaxOverMinNA,na.rm=TRUE)
apply(b,1,MaxOverMinNA)


# C. Dealing with NAs and zeroes
c=read.table("MaxOverMinC.txt",header=FALSE)
apply(c,1,MaxOverMinNA)

MaxOverMinNA_NaN<-function(x,na.rm=FALSE,zero.rm=FALSE){
	if(na.rm==TRUE){
		if(zero.rm==TRUE){
			max(x[x>0],na.rm=TRUE)/min(x[x>0],na.rm=TRUE)
		}else{
			max(x,na.rm=TRUE)/min(x,na.rm=TRUE)
		}
	}else{
		if(zero.rm==TRUE){
			max(x[x>0])/min(x[x>0])
		}else{
			max(x)/min(x)
		}
	}
}

apply(c,1,MaxOverMinNA_NaN)
apply(c,1,MaxOverMinNA_NaN,na.rm=TRUE)
apply(c,1,MaxOverMinNA_NaN,zero.rm=TRUE)
apply(c,1,MaxOverMinNA_NaN,na.rm=TRUE,zero.rm=TRUE)



# Question 2
# function for estimating slope from two points
# needs to return point coordinates, slope, and intercept
# if only one point provided, should use the origin as the second point

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

slopeCalc1(p1=c(1,1),p2=c(5,5))
slopeCalc1(p1=c(1,1))

data=read.table("points.txt",header=TRUE,sep="\t")

for(i in 1:nrow(data)){
	x=slopeCalc1(p1=data[i,1:2],p2=data[i,3:4])
	print(x)
}

# alternative that allows for apply
slopeCalc2<-function(p){
	if(length(p)==4){
		p1=p[1:2]
		p2=p[3:4]

		m=(p1[2]-p2[2])/(p1[1]-p2[1])
		b=p1[2]-(m*p1[1])
		
		out=list(p1=p1,p2=p2,b=b,m=m)
		return(out)
	}else if(length(p)==2){
		p1=p[1:2]
		p2=c(0,0)

		m=(p1[2]-p2[2])/(p1[1]-p2[1])
		b=p1[2]-(m*p1[1])
		
		out=list(p1=p1,p2=p2,b=b,m=m)
		return(out)		
	}else{
		return("Error in provided coordinates; must provide 2 or 4 numeric values")
	}
}

slopeCalc2(c(1,1,5,5))
slopeCalc2(rep(1,2))

z=apply(data,1,slopeCalc2)
str(z)

# avoiding lists
slopeCalc3<-function(p){
	if(length(p)==4){
		p1=p[1:2]
		p2=p[3:4]

		m=(p1[2]-p2[2])/(p1[1]-p2[1])
		b=p1[2]-(m*p1[1])
		
		out=c(p1,p2,b,m)
		names(out)=c('x1','y1','x2','y2','b','m')
		return(out)
	}else if(length(p)==2){
		p1=p[1:2]
		p2=c(0,0)

		m=(p1[2]-p2[2])/(p1[1]-p2[1])
		b=p1[2]-(m*p1[1])
		
		out=c(p1,p2,b,m)
		names(out)=c('x1','y1','x2','y2','b','m')
		return(out)		
	}else{
		return("Error in provided coordinates; must provide 2 or 4 numeric values")
	}
}

slopeCalc3(c(1,1,5,5))
slopeCalc3(c(1,1))

apply(data,1,slopeCalc3)