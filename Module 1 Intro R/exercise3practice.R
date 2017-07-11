## practice with functions and for loops using chlorophyll ##
#chl a in ug/L
chlorophyll <- c(146,130,65,78,98,190,200,300,450,120,50,20,20,20,20)
#function to convert ug/l to ug/cm^2
#apply function

chlorphyll.cm <- sapply(chlorophyll, FUN = function(i) (i*0.8*4*.1))
chlorphyll.cm

ug.cm <- function(chl){
   chl = numeric(length(chlorophyll))
    for(i in 1:length(chl)){
    chl[i] = (chlorophyll[i]*.8*4*.1)/1000
    }
   return(chl)
}

##practice matrix making with larval lengths over weeks##
ephydra <- matrix(c(12,13,14,15,15,16,17,17,8,17,17,16,16,16,18,17,13,15,16,17,19),nrow=4,ncol=5)
ephydra

#incremental growth across rows
inc = matrix(NA, nrow(ephydra), ncol(ephydra)-1)
for(i in 1:nrow(inc)){
  for(j in 2:ncol(ephydra)){
    inc[i, (j-1)] = ephydra[i,j]-ephydra[i,j-1]
  }
}

# calculate mean growth
meangrow <- numeric(nrow(inc))
  for(i in 1:length(meangrow)){
    meangrow[i] = mean(inc[i,])
}

