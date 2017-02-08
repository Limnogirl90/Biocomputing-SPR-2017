##9 by 9 multiplication table
> A = matrix(0, nrow=9,ncol=9)
> for(i in 1:9){
     for(j in 1:9){ ##nested for loops
          A[j,i]=j*i
      }
     }

###For loop for vector
x = runif(n=1e6,min=0,max=100)
y = runif(n=1e6,min=0, max=100)
z=numeric(length(x)) ## preallocating vector of length x
system.time(for(i in 1:length(x)){z[i]=x[i]+y[i]})

###Vectorization
###For loop for vector
x = runif(n=1e6,min=0,max=100)
y = runif(n=1e6,min=0, max=100)
system.time(z <- x+y)
