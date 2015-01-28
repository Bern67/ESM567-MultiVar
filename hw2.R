#Hw# 2

dta <- read.csv("evenv.csv")
env <- na.omit(dta)
rm(dta)
library(dplyr)
env <- select(env,-Site)
detach()

boxplot(env)
boxplot(scale(env))

#source cor.matrix function
cor.matrix(env)

round(var(env),2) #actual variance and covariance (not good, one variable not same unit)
round(var(scale(env)),2) #calculate variance and covariance matrix with the standardized data: Z-score from -1 to 1

require(MASS) #loads the PCA package
pca <- princomp(env, scores=TRUE,cor=TRUE) #creates a PC matrix using the correlation matrix
summary(pca) #proportion of variance is eigenvalues for each PC

plot(pca, main="Scree Plot") #Scree plot
broken.stick(15) #After comparing, keep comp 1 & 2

biplot(pca, main = "Biplot", xlab = "Comp.1 (26.3%)", ylab = "Comp.2 (23.8%)")
loadings(pca) #Check eigenvector elements: How closely variables and components are related; Principal component loading (pg 50)
pc <- round((pca$scores),2) #Shows the rotated/transformed dataset: PC matrix
pc




#plaing with vegan package
library(vegan)
ord <-metaMDS(env)
plot(ord, disp="sites", type="n")
ordihull(ord, Management, col="blue")
ordiellipse(ord, Management, col=3,lwd=2)
ordispider(ord, Management, col="red", label = TRUE)
points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)
