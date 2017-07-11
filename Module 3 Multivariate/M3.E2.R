###M3 Exercise 2###
##1. Which one when? Compare and contrast the four indirect ordination method discussed this week. 
#Provide an example of a dataset that would be well suited for each method. 

  #A PCA: eigenvector-based, preserves Euclidean distances and therefore requires quantitative data.
#chemical data, environemntal variables

  #B PCoA: eigenvector-based, can preserve any distance metric, can handle quantitative, binary, semi-quant data
#species data-raw counts would be appropriate for this analysis

  #C CA: eigenvector-based, preserves chi-square distances and assumes unimodal distribution of descriptors along 
#environmental gradients, and horse-shoe effect can occur

  #D nmDS: not eigenvector-based, but rather focused on "rankings" in ordination space. Because distances are not
#preserved, the ordination can be rotated, scaled, and inverted along its axes without loss of interpretation
#good for species composition data and environmental variables and treatments, factors

##2. Soil chemical characteristics across regions You've been provided soil chemistry data ('soilchem.txt') 
#from eight sites in four regions of Indiana (northeast, northwest, southwest, and southeast). 
#Do the sites within a region tend to be more chemically similar than sites across regions? 
#What are the chemical characteristics, if any, that most strongly differ across regions? 
#How many axes explain meaningful variation in the chemical data? 

#load in data

soil <- read.table("soilchem.txt",header=T,row.names=NULL)
region <- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8))
site <- c(rep(1:8, each=4))
soil <- soil[-1]
soil$region <- region
soil$site <- site
soil.std <- decostand(soil, method='standardize', MARGIN = 2)

#Association matrix
soil.dist <- vegdist(soil.std, method='euclidean')
soil.pcoa <- as.data.frame(cmdscale(soil.dist)) ##cmdscale does pcoa
summary(soil.pcoa)
soil.clust <- hclust(soil.dist, "complete")
plot(soil.clust)

plot(soil.pcoa, main= "Soil chemistry")
points(soil.pcoa[soil.pcoa$V1 == '1' & soil.pcoa$V2=='2',1],soil.pcoa[soil.pcoa$V1 == '1' & soil.pcoa$V2=='2',2],pch=1,col=1)
ordihull(soil.pcoa, groups = region,col=c(1:4)) ##coded by region
soil.fit <- envfit(soil.pcoa, soil[,1:8],permu = 999) 
plot(soil.fit, p.max = 0.1)
legend('topright', legend = c(1,2,3,4,5,6,7,8), col = c(1:8), cex = 0.8, pch = 1)
adonis(soil~region*site)
#according to adonis, region is significantly different among sites, but sites are not significant

soil.rda <- rda(soil.pcoa) #pca object
soil.pca <- prcomp(soil)
soil.rda <- rda(soil)
col.region = c(1:4)
points(soil.rda,display ="site",pch = 1, col = col.region[region], cex=1.5)

#How many axes explain significant variation in soil chem?
ev.soil = soil.rda$CA$eig
ev.soil

soil.axes<- ev.soil[ev.soil > mean(ev.soil)] #PC1 explains most variation, 94.3% of variation
soil.axes/sum(ev.soil)

# Plot eigenvalues and % of variance for each axis
dev.new(title="PCA eigenvalues for Soil")
barplot(ev.soil, main="Eigenvalues", col="bisque", las=2)
abline(h=mean(ev.soil), col="red")	# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

#load in nematode data
nematode <- read.table("nematodeData.txt",header=T, row.names = NULL)
nematode
dim(nematode)
region <- c(rep(1,8),rep(2,8),rep(3,8),rep(4,8))
site <- c(rep(1:8, each=4))
nematode <- nematode[-1]
dim(nematode)
nematode$region = region
nematode$site = site
nema.nmds <- metaMDS(nematode[,1:50], distance='bray')
plot(nema.nmds)
dim(nematode)
#plotting nematode communities by site
plot(nema.nmds$points[,2],nema.nmds$points[,1], type="n", main="Nematode Communities by Site",xlab="NMDS Axis 1",ylab="NMDS Axis 2")
ordisymbol(nema.nmds,nematode,factor="site",cex=1.25, rainbow=T,legend=T)

#plotting nematode communities by region
plot(nema.nmds$points[,2],nema.nmds$points[,1], type="n", main="Nematode Communities by Region",xlab="NMDS Axis 1",ylab="NMDS Axis 2")
ordisymbol(nema.nmds,nematode,factor="region",cex=1.25, rainbow=T,legend=T)
library(BiodiversityR)
#Region separates out better than site

##distance and adonis on nematodes
nematode.dist <- vegdist(nematode,method='bray')
adonis(nematode~site*region, nematode)
nematode.fit <- envfit(nema.nmds,nematode,permutations=999)
plot(nematode.fit,p.max=0.05)
soil.fit <- envfit(nema.nmds, soil[,1:8],permu = 999) 
plot(soil.fit, p.max = 0.1)
soil.fit
#Looks like about half of the nematode communities are associated with phosphate, K, pH, Ca, while the other 
#half is characterized by NH4, Cl, and NO3.

#how many axes explain meaningful variation in nematode composition? Looks like 6 PC explain variation
# Eigenvalues
nema.pca <- rda(nematode,method='bray')
ev.nema <- nema.pca$CA$eig
ev.nema
# Apply Kaiser-Guttman criterion to select axes
nema.axes <- ev.nema[ev.nema > mean(ev.nema)]
nema.axes/sum(ev.nema)


# Plot eigenvalues and % of variance for each axis, however there are some eigenvalues that drive down the variance explained
dev.new(title="PCA eigenvalues Nematode")
barplot(ev.nema, main="Eigenvalues Nematode", col="bisque", las=2)
abline(h=mean(ev.nema), col="red")	# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

#4 select article, describe ordination and data it is based on, any multivariate hypothesis testing techniques?
#Microbialite response to an anthropogenic salinity gradient in Great Salt Lake, Utah, Lindsay et al. 2016, Geobiology
#PCA ordination of the dissimilarity (Bray-Curtis) in the composition and intensity of GeoChip functional gene probes
#in North Arm and South Arm microbialite genomic DNA extracts. Data is based on 939 unique genes detected in 
#microbialites