#  tadpole survivorship
# Plot the # of tadpoles consumed as a function of initial density
tadpoles=read.table("overnightSurvivorship.txt",header=TRUE,sep="\t")

consumed=tadpoles[,1]-tadpoles[,2]
plot(tadpoles[,1],consumed,pch=16,cex=4)

# Fit Type II functional response model for binomial probability of death

points(lci$y,lci$x,pch=15,cex=3)

prof.upper=aprof[which.min(aprof):100]
prof.avec=avec[which.min(aprof):100]
uci=approx(prof.upper,prof.avec,xout=(mfit$value+qchisq(0.95,1)/2))

points(uci$y,uci$x,pch=15,cex=3)

mfit2=optim(par=c(a=1,b=3,shape=50),fn=mNLL,time=m[,1],titer=m[,2],hessian=TRUE)
mfit2
ses=sqrt(diag(solve(mfit2$hessian)))*qnorm(0.975)

abline(v=mfit2$par[1]-ses[1],lwd=4,lty=2,col='red')
abline(v=mfit2$par[1]+ses[1],lwd=4,lty=2,col='red')