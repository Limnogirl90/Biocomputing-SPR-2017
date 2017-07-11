## W-L Lakes fish data##

fish.age <- read.csv("fish.lengths.csv", header =TRUE, stringsAsFactors = FALSE)
fish.age <- data.frame(fish.age)
which(fish.age$Species == "YP")

fish.age$Species[which(fish.age$Species == "YP")]

max(fish.age$Age[which(fish.age$Species=="YP")])

species <- fish.age$Species
ages <- fish.age$Age
year <- fish.age$Year

aov1 <- aov(fish.age$Age[which(fish.age$Species=="YP")]~fish.age$Lake[which(fish.age$Species=="YP")])
#need a better way to automate this

table.fish <- cbind(species, ages, year)
#subset by species still want to figure out


table 
##write to a new file#

#use an apply function#
#simple for loop##

b <- sample(1:100, 50)
c <- length(b)

for(i in 1:length(b)){
  c[i]=sum(b[1:i])
}

#practicing grep

practice <- c('cat', 'CAT', 'DOG', 'dOg')
grep('[a-zA-Z][A-Z][a-zA-Z]', practice, value=T)
grep('[A-Z][A-Z][A-Z]', practice, value=T)

#practicing apply

v <- c(1:90)
v.2 <- sapply(v, FUN = function(i) (i*2)/4)

m <- matrix(5,30,nrow=6,ncol=5)
m
#going back to tree dbh data

dbh <- read.table("treeDBH.txt", stringsAsFactors = FALSE, header = T)
dbh

inc <- matrix(NA, nrow(dbh), ncol(dbh)-1)
for(i in 1:nrow(inc)){
  for(j in 2:ncol(dbh)){
    inc[i,(j-1)] = dbh[i,j]- dbh[i,(j-1)]
  }
}

meanGrowth <- numeric(nrow(inc))
for(i in 1:length(meanGrowth)){
  meanGrowth[i] = mean(inc[i,])
}


