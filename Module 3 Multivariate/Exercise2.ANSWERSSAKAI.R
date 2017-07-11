# Module3 - Exercise 2: Indirect Ordination
# SEJ

# 1) Compare and contrast the four indirec t ordination methods. Provide an example of when it would be most useful.

# a. Principle Components Analysis (PCA)
# - preserves euclidean distances
# - eigenvector-based
# - linear relationships
# - chemical data

# b. Principle Coordinates Analysis (PCoA)
# - can use any association metric
# - eigenvector-based
# - assumes linear relationships
# - Ordinating species data

# c. Correspondence Analysis (CA)
# - based upon chi-squared distance
# - generates/assumes unimodal relationship of features to gradients
# - eigenvector-based
# - requires 0 or positive integers
# - Ordinating species data on "long gradients"

# d. Non-metric multidimensional scaling (NMDS)
# -not based upon eigenvectors
# -preserves order not distance
# -axes are less meaningful/useful than other ordination techniques
# -flexible like PCoA (can use any association metric)
# -Ordinating species data, but have no covariate information


# 2) Soil chemistry across regions
library(vegan)
sc=read.table("soilchem.txt",header=TRUE,row.names=1,sep="\t")
sc.z=decostand(sc,method="standardize",MARGIN=2)
sc.pca=rda(sc.z)
sc.sc=scores(sc.pca)

plot(sc.sc$sites[,1],sc.sc$sites[,2],pch=rep(20:23,each=8),cex=3,lwd=3,xlab="PCA 1",ylab="PCA 2") # the sites seem to weakly cluster by region

plot(sc.pca)		# region 2 (and maybe 4) may have higher nutrients; regions 3 and 4 seem to have higher pH and cations

sc.eig=sc.pca$CA$eig
mean(sc.eig)		
sum(sc.eig>mean(sc.eig))	# 4 axes explain meaningful variation based on Kaiser-Guttman criterion

summary(sc.pca)


# 3) Soil nematodes across regions
nm=read.table("nematodeData.txt",header=TRUE,row.names=1,sep="\t")

nm.rel=decostand(nm,method="total",MARGIN=2)

nm.rel.d=vegdist(nm.rel)
nm.rel.pcoa=cmdscale(nm.rel.d,eig=TRUE)

nm.d=vegdist(nm)
nm.pcoa=cmdscale(nm.d,eig=TRUE)
nm.pcoa2=cmdscale(nm.d,eig=TRUE,add=TRUE)		# add a minimum constant to distances to make euclidean (Cailliez correction)

plot(nm.pcoa$points,pch=rep(20:23,each=8),cex=3,lwd=3,xlab="PCoA 1",ylab="PCoA 2")	# yes weakly groups by region
legend('topleft',c("region 1","region 2","region 3","region 4"),pch=20:23,box.lty=0)
nm.pcoa.ef=envfit(nm.pcoa,env=sc.z)
plot(nm.pcoa.ef)

nm.eig=nm.pcoa$eig
nm.eig/sum(nm.eig)
nm.eig2=nm.pcoa2$eig
nm.eig2/sum(nm.eig2)
sum(nm.eig2>mean(nm.eig2))	# with correction 8 axes

nm.round=round(nm)
nm.ca=cca(nm.round)
plot(nm.ca)
plot(nm.ca,scaling="none")
# region 3 tends to have more of species 40, 5, 34, 1, and 21
# region 1 tends to have more of species 44, 19, and 12

nm.ca.ef=envfit(nm.ca,env=sc.z)
plot(nm.ca.ef)

nm.ca.eig=nm.ca$CA$eig
sum(nm.ca.eig>mean(nm.ca.eig))
nm.ca.varexp=nm.ca.eig/sum(nm.ca.eig)
sum(nm.ca.varexp[1:2])	# 56% explained in axis 1 and 2


# 4) Find a paper
# What was data ordinated?
# What ordinatino?
# Appropriate?
# Hypothesis testing?
# What test?