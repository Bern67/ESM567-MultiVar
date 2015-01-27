#Hw# 2

dta <- read.csv("evenv.csv")
dta <- na.omit(dta)
library(dplyr)
dta <- select(dta,-Site)
detach()

boxplot(dta)
boxplot(scale(dta))

#source cor.matrix function
source(cor.matrix.r)
cor.matrix(dta)

round(var(scale(dta)),2) #calculate variance and covariance matrix with the standardized data

require(MASS) #loads the PCA package
pca <- princomp(dta, scores=TRUE,cor=TRUE) #creates a PC matrix using the correlation matrix
summary(pca) #proportion of variance is eigenvalues for each PC
plot(pca) #Scree plot

source(broken.stick.r)
broken.stick(15) #After comparing, keep comp 1 & 2
biplot(pca, pc.biplot = FALSE)
loadings(pca) #Check eigenvectors: How closely a component and variables are related
pca$scores #Shows the rotated/transformed dataset: PC matrix

