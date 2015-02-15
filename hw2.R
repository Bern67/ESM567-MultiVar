#Hw# 2

dta <- read.csv("evenv.csv")
env <- na.omit(dta)
rm(dta)
env <- env[,-1]


#-----
norm <- par(mfrow=c(4,4))
qqnorm(env$TPeleo, main="TPeleo")
qqline(env$TPeleo)
qqnorm(env$TNeleo, main="TNeleo")
qqline(env$TNeleo)
qqnorm(env$TPsed, main="TPsed")
qqline(env$TPsed)
qqnorm(env$TCsed, main="TCsed")
qqline(env$TCsed)
qqnorm(env$TNsed, main="TNsed")
qqline(env$TNsed)
qqnorm(env$AFDMsed, main="AFDMsed")
qqline(env$AFDMsed)
qqnorm(env$CHLAsed, main="CHLAsed")
qqline(env$CHLAsed)
qqnorm(env$NH4.N, main="NH4.N")
qqline(env$NH4.N)
qqnorm(env$NO3.N, main="NO3.N")
qqline(env$NO3.N)
qqnorm(env$FILT.TN, main="FILT.TN")
qqline(env$FILT.TN)
qqnorm(env$UNFILT.TN, main="UNFILT.TN")
qqline(env$UNFILT.TN)
qqnorm(env$FILT.PO4, main="FILT.PO4")
qqline(env$FILT.PO4)
qqnorm(env$FILT.TP, main="FILT.TP")
qqline(env$FILT.TP)
qqnorm(env$SIO2, main="SIO2")
qqline(env$SIO2)
qqnorm(env$UNFILT.TP, main="UNFILT.TP")
qqline(env$UNFILT.TP)
par(norm)

boxplot(env, main = "Not scaled")
boxplot(scale(env), main="Scaled with Z-score")
boxplot(scale(log(env+1),main="log transformed"))

#source cor.matrix function
cor.matrix(scale(env))

cov(scale(env)) #calculate correlatin matrix with the standardized data: 
#Z-score from -1 to 1 (PCC)

ev.tn <- env[,c(5,11,12,14)]
cor.matrix(ev.tn)
ev.tp <- env[,c(1,3,4,15)]
cor.matrix(ev.tp)

round(cov(env),2) #correlation matrix
diag(round(cor(env),2)) #show variance only from correlation matrix (along diagonal)


#----

require(MASS) #loads the PCA package
pca <- princomp(scale(log(env+1))) #creates a PC matrix using the correlation matrix
biplot(pca, expand = 1.05,main = "Biplot", xlab = "Comp.1 (27.2%)", ylab = "Comp.2 (23.4%)")
#Scale for sites(PC matrix-pca$scores) on top, scale for variables (vectors-loadings) along bottom
summary(pca) #proportion of variance is eigenvalues for each PC

library(vegan)
screeplot(pca, bstick = TRUE) #inertia= variance n PCA

round(loadings(pca),2) #Check eigenvectors: length of vector is relative variance and how much it contributes to the PC
#Principal component loading (pg 50).  The further from zero, the greater the contribution.
round(loadings(pca)[,c(1:2)],2) #Loading for PC1 & 2 only

round((pca$scores),2) #PC matrix showing site scores for all PCs. How far each is(SD) from the the grand centroid
#This is the distribution of PC1 and PC2 site scores (top scale).  Each variable for each component. 
#In this case due to broken stick, PC1 and PC2

#---------
#create shepard diagram (data used in PCA)
euc<-dist(scale(log(env+1))) #Calculate Euclidian distance among sites centered=scale to Z-score (multidimentional spcae)
euc.1<-dist(pca$scores[,c(1,2)]) #calculate Euclidian distance among sites in PCA space using only first 2 PCs (reduced space)
plot(euc,euc.1,main="PC=2", xlab="Distance in Multidimensional space", ylab="Distance in Reduced space")  

