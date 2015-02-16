#Bern Romey 08Feb15, hw3pca portion

#DATA
#----
dta <- read.csv("wemap_pnw_rda_HW.csv")
wtr <- na.omit(dta) #remove missing data
wq <- wtr[c(2:20)] #water quality variables.
wq.l <- log(wq+1)

#Normality
#----
cor.matrix(wq)
cor.matrix(wq.lg)#log transformed is much better for normality.  
#Most of the data is skewed to the right, log transformation fixes that.

round(cor(wq.l),2)#Correlatin matrix of log transformed water quality data.  Cor centers data to Z-score.

#----
#PCA
#----
require(MASS) #loads the PCA package
pca <- princomp(wq.l, cor=TRUE) #creates a PC matrix using the correlation matrix
biplot(pca, main = "Biplot", xlab = "Comp.1 (46.4%)", ylab = "Comp.2 (14.7%)")
#Scale for sites(PC matrix-pca$scores) on top, scale for variables (vectors-loadings) along bottom
summary(pca) #proportion of variance is eigenvalues for each PC
# How well these PCs represent the original data in term of variance and why? (eigenvalues)

library(vegan)
screeplot(pca, bstick = TRUE, main="PCA", npcs = 8) #inertia= variance in PCA

round(loadings(pca),2) #Check eigenvectors: All very similar, look at groups instead
#What does each PC mean in terms of original variables? (eigenvectors)
round(loadings(pca)[,c(1:2)],2) #Loading for PC1 & 2 only

pca$scores[,1]
hist(pca$scores[,1])

#Shepard diagram
##calculate Euclidean distance among sites
#The plot shows the distortion that occurs when converting from multidimentional space to two dimentions.

euc<-dist(scale(wq.l)) #Calculate Euclidian distance among sites scale=centered to Z-score (multidimentional spcae). Check transformation and matrix used.
euc.1<-dist(pca$scores[,c(1,2)]) #calculate Euclidian distance among sites in PCA space using only first 2 PCs (reduced space).
plot(euc,euc.1,main="Shepard Diagram (PC=2)", xlab="Distance in Multidimensional space", ylab="Distance in Reduced space") #x=euc, y=euc.1  


#Homework Questions:
#What are spatial patterns/trends among sites in the reduced PCA space?
#(bubble plots, use other categorical grouping variables, etc.)  

#Regress PC 1 with watershed variables (cor.matrix)- Lec 7, slide 16
#use principal component scores and merge with watershed df.
ws <-wtr[c(21:34)]
ws.l <-scale(log(ws+1)) #watershed variables
pc1 <- pca$scores[,1]
pc1<-as.data.frame(pc1)
crmx <-cbind(pc1,ws.l)
cor.matrix(crmx)

