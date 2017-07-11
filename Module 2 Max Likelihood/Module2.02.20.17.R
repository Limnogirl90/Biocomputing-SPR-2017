##Module 2 Maximum Likelihood##
#simulate data
#t-test approach

B0 = 9 #ft
B1 = 13 #ft the difference between the 2 tree heights

x=c(rep(0,5),rep(1,5))
height=B0+B1*x + rnorm(length(x),0,2)
##adding error term at end of linear model accounts for noise and this error term comes from a distribution controlled 
#by parameter sigma
boxplot(height~x)
y=lm(height~x)
summary(y)

##simple linear regression
B0 = 1 #m3 m-3
B1 = 0.1 #ft (m3 m-3)-1 over water content
x=runif(10,min=0,max=100) #simulated water content for soil
x

#simulate height data
height = B0+B1*x+rnorm(length(x),0,2)
plot(x,height,pch=20)
fitregression=lm(height~x)
summary(fitregression)
#when intercept isn't significant, means that you can't tell the difference between intercept and 0