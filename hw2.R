#Hw# 2

dta <- read.csv("evenv.csv")
env <- na.omit(dta)
rm(dta)
env <- env[,-1]

boxplot(env, main = "Not scaled")
boxplot(scale(env), main="Scaled with Z-score")

#source cor.matrix function
cor.matrix(env)

round(var(scale(env)),2) #calculate variance and covariance matrix with the standardized data: Z-score from -1 to 1

require(MASS) #loads the PCA package
pca <- princomp(env, cor=TRUE) #creates a PC matrix using the correlation matrix
summary(pca) #proportion of variance is eigenvalues for each PC

plot(pca, main="Scree Plot") #Scree plot
broken.stick(15) #After comparing, keep comp 1 & 2

par(mfrow=c(1,2))
biplot(pca, expand = 1.05,main = "Biplot", xlab = "Comp.1 (26.3%)", ylab = "Comp.2 (23.8%)")
loadings(pca) #Check eigenvector elements: How closely variables and components are related; Principal component loading (pg 50)
round((pca$scores),2) #Shows the rotated/transformed dataset: PC matrix

#plaing with vegan package
library(vegan)
mod <-rda(env, scale = TRUE)
biplot(mod, scaling = 3)
biplot(mod, scaling = 3, type = c("text", "points"))
summary(mod)
