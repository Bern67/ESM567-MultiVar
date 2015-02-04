#HW3

dta <- read.csv("wemap_pnw_rda_HW.csv")
pnw <- na.omit(dta) #remove missing data
rm(dta)

wq <- pnw[c(2:9, 11:20)] #water quality variables
ws <-pnw[c(21,23:24,26:32)] #watershed variables

#make sure to scale data
boxplot(scale(log(wq+1))) 
boxplot(scale(ws))

cor.matrix(wq)
cor.matrix(ws)

wq.lg <-log(wq+1)
cor.matrix(wq.lg)

ws.lg <-log(ws+1)
cor.matrix(ws.lg)

lshap <- lapply(ws.lg, shapiro.test) #shapiro test on log transformed data
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)

#WATER QUALITY MATRIX
round(var(wq.lg),2) #covariance matrix
diag(round(var(wq.lg),2)) #show variance only from covariance/var matrix (along diagonal)

require(MASS) #loads the PCA package
pca <- princomp(scale(wq.lg)) #creates a PC matrix using the correlation matrix
biplot(pca, main = "Biplot", xlab = "Comp.1 (48.8%)", ylab = "Comp.2 (14.8%)")
#Scale for sites(PC matrix-pca$scores) on top, scale for variables (vectors-loadings) along bottom
summary(pca) #proportion of variance is eigenvalues for each PC

plot(pca, main="Scree Plot") #Scree plot
broken.stick(18) #After comparing, keep comp 1 & 2

round(loadings(pca),2) #Check eigenvectors:

#WATERSHED MATRIX
round(var(ws),2) #covariance matrix
diag(round(var(ws),2)) #show variance only from covariance/var matrix (along diagonal)

require(MASS) #loads the PCA package
pca <- princomp(scale(ws)) #creates a PC matrix using the correlation matrix
biplot(pca, main = "Biplot", xlab = "Comp.1 (48.8%)", ylab = "Comp.2 (14.8%)")
#Scale for sites(PC matrix-pca$scores) on top, scale for variables (vectors-loadings) along bottom
summary(pca) #proportion of variance is eigenvalues for each PC

plot(pca, main="Scree Plot") #Scree plot
broken.stick(18) #After comparing, keep comp 1 & 2

round(loadings(pca),2) #Check eigenvectors:
