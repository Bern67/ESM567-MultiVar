#Hw# 2

dta <- read.csv("evenv.csv")
dta <- na.omit(dta)
library(dplyr)
dta <- select(dta,-Site)
dta <- filter(dta, CHLAsed>0) # need to remove zeros if log transforming data

boxplot(dta)
boxplot(scale(dta))
boxplot(log(scale(dta)))

#run cor.matrix function
cor.matrix(dta)
var(scale(dta)) #calculate variance and covariance matrix with the standardized data


require(MASS) #loads teh PCA package
pca <- princomp(scale(dta[,-c(1,9)])) #creates a PC matrix
pca$scores # Show PC matrix
summary(pca) #proportion of variance is eigenvalues for each PC

broken.stick(13) #After comparing, keep comp 1 & 2
biplot(pca)
loadings(pca)
