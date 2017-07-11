### Module 3 - Exercise 3 
### SEJ

setwd("/Users/stuartjones/Documents/Teaching/AdBiostats/AdBiostats2017/Module03-MultavariateStatistics/Exercise03")

library(vegan)

# 1) Skink populations
skink=read.table("islandMicrosats.txt",header=TRUE,row.names=1,sep="\t")

skink.dist=vegdist(skink,method="jaccard")
skink.pcoa=cmdscale(skink.dist,eig=TRUE)

# plotting
plot(skink.pcoa$points[,1],skink.pcoa$points[,2],pch=rep(20:24,each=10),lwd=2,cex=4)

plot(skink.pcoa$points[,1],skink.pcoa$points[,2],cex=0)
text(skink.pcoa$points[,1],skink.pcoa$points[,2],rep(1:5,each=10),cex=5)

skink.clust=hclust(skink.dist)
plot(skink.clust)

# hypothesis testing
anosim(skink.dist,grouping=rep(1:5,each=10))
mrpp(skink.dist,grouping=rep(1:5,each=10))
adonis(skink.dist~as.factor(rep(1:5,each=10)))	#### note as.factor!!!!

anosim(skink.dist,grouping=c(rep(1,10),rep(2,30),rep(1,10)))


# 2) Probiotics and diet
pro=read.table("probiotic&diet.txt",header=TRUE,row.names=1,sep="\t")
rowSums(pro)
proRel=decostand(pro,method="total",MARGIN=1)
pro.dist=vegdist(proRel)

pro.pcoa=cmdscale(pro.dist)

# ordination
plot(pro.pcoa[,1],pro.pcoa[,2],pch=rep(20:23,each=8),lwd=2,cex=8)
legend('')
# design matrix
pro.design=matrix(0,nrow(pro),2)
pro.design[c(9:16,25:32),1]=1
pro.design[17:32,2]=1
rownames(pro.design)=rownames(pro)
colnames(pro.design)=c('probiotic','vegan')

# hypothesis testing
adonis(pro.dist~as.factor(pro.design[,1])*as.factor(pro.design[,2]))

pro.rda=rda(proRel~pro.design[,1]*pro.design[,2])
plot(pro.rda)

# 3) Centralia microbes
microbes=read.table("Centralia_Crobes.txt",header=TRUE,row.names=1,sep="\t")
env=read.table("Centralia_Env.txt",header=TRUE,row.names=1,sep="\t",stringsAsFactors=FALSE)

microbesHel=decostand(microbes,method="hellinger",MARGIN=1)
microbes.pca=rda(microbesHel)
microbes.sc=scores(microbes.pca)$sites

sitePCH=rep(20,nrow(env))
sitePCH[env$Classification=="FireAffected"]=22
sitePCH[env$Classification=="Recovered"]=24

plot(microbes.sc[,1],microbes.sc[,2],pch=sitePCH,lwd=2,cex=4)
legend('topright',c('Reference','Fire Affected','Recovered'),pch=c(20,22,24),box.lty=0)

# a) difference amongst site classfication?
anosim(dist(microbesHel),grouping=env$Classification)
adonis(microbesHel~env$Classification)

# b) what environmental variables are important?
envZ=decostand(env[,-1],method="standardize",MARGIN=2)
centralia.rda=rda(microbesHel~.,envZ)
anova.cca(centralia.rda)

coef(centralia.rda)[,1:2]
cor(envZ)

# c) classification or continuous environment better descriptor?
centralia.rda.Class=rda(microbesHel~as.factor(env[,1]))

centralia.rda.Class		# 0.2721 proportion of inertia constrained
centralia.rda			# 0.6992 proportion of inertia constrained

# d) visualize indirect vs. direct
plot(microbes.sc[,1],microbes.sc[,2],pch=sitePCH,lwd=2,cex=4)
legend('topright',c('Reference','Fire Affected','Recovered'),pch=c(20,22,24),box.lty=0)

dev.new()
centralia.sc=scores(centralia.rda)$sites
plot(centralia.sc[,1],centralia.sc[,2],pch=sitePCH,lwd=2,cex=4)
legend('topright',c('Reference','Fire Affected','Recovered'),pch=c(20,22,24),box.lty=0)
