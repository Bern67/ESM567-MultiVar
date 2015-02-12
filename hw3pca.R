#Bern Romey 08Feb15, hw3pca portion

dta <- read.csv("wemap_pnw_rda_HW.csv")
pnw <- na.omit(dta) #remove missing data
rm(dta)
str(pnw)

wtr <- pnw[c(2:9, 11:20)] #numeric variables. Watershed variables (21,23:24,26:32)
wtr.lg <- log(wtr+1)

cor.matrix(wtr)
cor.matrix(wtr.lg)

lshap <- lapply(wtr, shapiro.test) #shapiro test on log transformed data
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)

lshap <- lapply(wtr.lg, shapiro.test) #shapiro test on log transformed data
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)

round(cor(wtr.lg),2) #correlation Matrix
diag(round(cov(wtr.lg),2)) #show variance only from correlation matrix (along diagonal)

#PCA
require(MASS) #loads the PCA package
pca <- princomp(wtr.lg, cor=TRUE) #creates a PC matrix using the correlation matrix
biplot(pca, main = "Biplot", xlab = "Comp.1 (48.8%)", ylab = "Comp.2 (14.8%)")
#Scale for sites(PC matrix-pca$scores) on top, scale for variables (vectors-loadings) along bottom
summary(pca) #proportion of variance is eigenvalues for each PC
# How well these PCs represent the original data in term of variance and why? (eigenvalues)

broken.stick(18) #After comparing, 1 & 2 above broken.strick
plot(pca, main="Scree Plot") #Scree plot

round(loadings(pca),2) #Check eigenvectors: All very similar, look at groups instead
#What does each PC mean in terms of original variables? (eigenvectors)

pca$scores

##calculate Euclidean distance among sites
wtr.d<-round(var(scale(wtr.lg,scale=F)),0)  #calculate variance-covariance matrix and save it to 'wtr.d'
e.1<-eigen(wtr.d) #eigen-analysis
pc.matrix.1<-(as.matrix(scale(wtr.lg, scale=F)))%*%e.1$vectors #calculate pc.matrix
euc<-dist(scale(wtr.lg, scale=F))  #calculate Euclidian distance among site for centered original data
round(euc,2)
euc.1<-dist(pc.matrix.1[,c(1,2)])  #calculate Euclidian distance among sites in PCA space using only first 2 PCs
round(euc.1,2)
plot(euc,euc.1,main="Shepard diagram", xlab="Distance in Multidimensional space", ylab="Distance in Reduced space")
#The plot shows the distortion that occurs when converting from multidimentional space to two dimentions.

#What are spatial patterns/trends among sites in the reduced PCA space?
#(bubble plots, use other categorical grouping variables, etc.) 



#Regress PC 1 or 2 with additional explanatory variables   


#-------



