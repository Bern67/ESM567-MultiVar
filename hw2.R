#Hw# 2

dta <- read.csv("evenv.csv")
env <- na.omit(dta)
rm(dta)
env <- env[,-1]

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
boxplot(scale(log(env+1)))

#source cor.matrix function
var(scale(env)) #calculate correlatin matrix with the standardized data: 
#Z-score from -1 to 1 (PCC)
cor.matrix(scale(env))
cor.matrix(log(env+1))

round(var(env),2) #covariance matrix
diag(round(var(env),2)) #show variance only from covariance/var matrix (along diagonal)

require(MASS) #loads the PCA package
pca <- princomp(scale(env)) #creates a PC matrix using the correlation matrix
pca.l <-princomp(scale(log(env+1)))
biplot(pca, expand = 1.05,main = "Biplot", xlab = "Comp.1 (26.3%)", ylab = "Comp.2 (23.8%)")
#Scale for sites(PC matrix-pca$scores) on top, scale for variables (vectors-loadings) along bottom
biplot(pca.l)
summary(pca) #proportion of variance is eigenvalues for each PC

plot(pca, main="Scree Plot") #Scree plot
broken.stick(15) #After comparing, keep comp 1 & 2

round(loadings(pca),2) #Check eigenvectors: length of vector is relative variance and how much it contributes to the PC
#Principal component loading (pg 50).  The further from zero, the greater the contribution.
round(loadings(pca)[,c(1:2)],2) #Loading for PC1 & 2 only

round((pca$scores),2) #PC matrix showing site scores for all PCs. 
#This is the distribution of PC1 and PC2 site scores (top scale).  Each variable for each component. 
#In this case due to broken stick, PC1 and PC2


