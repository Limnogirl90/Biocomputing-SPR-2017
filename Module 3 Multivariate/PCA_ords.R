###Fish data for ordination methods CLASS 4.12.17##
env <- read.csv("DoubsEnv.csv",row.names=1)
env
spe <- read.csv("DoubsSpe.csv", row.names=1)
spe

#PCA on full dataset##
 env.pca <- rda(env, scale=T)
 env.pca
 #scaling reduces the inertia, makes it scaled to # of descriptors, 11, standardization of the variables
 #11 eigenvectors for the 11 variables
 summary(env.pca)
 (ev <- env.pca$CA$eig) #pulls out eigenvalues and saves as an object so can use it
 Varex <- ev/sum(ev)*100
 Varex
 
 #How many axes are enough? Kaiser-Guttman criterion
 ev[ev>mean(ev)] # eigenvalue greater than the mean is necessary to keep the eigenvalue
 #ranked in order of importance
 #can use this criterion for any of the eigenvector-based
 
 ##Plots using biplot.rda
 dev.new(title="PCA biplots",12,6)
 par(mfrow=c(1,2))
 plot(env.pca, scaling=1)
 plot(env.pca, main="PCA biplot2")
 
 ##PCoA##
 #cmdscale attempts to preserve whichever association metric we give it
 spe.bray = vegdist(spe)
 spe.b.pcoa = cmdscale(spe.bray,k=(nrow(spe)-1),eig=T)#this says give me all the eigenvalues
 #forcing non-euc into euclidean space, why warning occur, or transform association matrix
 dev.new(title="PCoA on Fish Species-BC")
 par(mfrow=c(1,1))
 ordiplot(scores(spe.b.pcoa)[,c(1,2)], type="t",display="sites", main="PCoA with species")
 abline(h=0, lty=3)
 abline(v=0, lty=3)
 # Add species by considering associations between predictors and the object-based ordiation axes
 spe.wa <- wascores(spe.b.pcoa$points[,1:2], spe)
 text(spe.wa, rownames(spe.wa), cex=0.7, col="red")
 
 ##CA scale by total inertia to get total variance explained
 spe.nmds = metaMDS(spe, distance="bray")
 spe.nmds
 spe.nmds$stress
 plot(spe.nmds)
 
 