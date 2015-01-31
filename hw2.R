#Hw# 2

dta <- read.csv("evenv.csv")
env <- na.omit(dta)
rm(dta)
env <- env[,-1]

boxplot(env, main = "Not scaled")
boxplot(scale(env), main="Scaled with Z-score")

#source cor.matrix function
var(scale(env)) #calculate correlatin matrix with the standardized data: 
#Z-score from -1 to 1 (PCC)
cor.matrix(scale(env))

round(var(env),2) #covariance matrix
diag(round(var(env),2)) #show variance only from covariance/var matrix (along diagonal)

require(MASS) #loads the PCA package
pca <- princomp(env, cor=TRUE) #creates a PC matrix using the correlation matrix
biplot(pca, expand = 1.05,main = "Biplot", xlab = "Comp.1 (26.3%)", ylab = "Comp.2 (23.8%)")
#Scale for sites on top, scale for variables (vectors) along bottom
summary(pca) #proportion of variance is eigenvalues for each PC

plot(pca, main="Scree Plot") #Scree plot
broken.stick(15) #After comparing, keep comp 1 & 2

round(loadings(pca),2) #Check eigenvectors: How much variability each variable contributes to components; 
# Principal component loading (pg 50).  The further from zero, the greater the contribution.
round(loadings(pca)[,c(1:2)],2) #Loading for PC1 & 2 only

round((pca$scores),2) #PC matrix showing site scores for all PCs. 

require(BiplotGUI)
Biplots(env)

