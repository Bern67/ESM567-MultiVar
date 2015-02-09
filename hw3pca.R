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

plot(pca, main="Scree Plot") #Scree plot
str(wtr)
broken.stick(18) #After comparing, 1 & 2 above broken.strick 

round(loadings(pca),2) #Check eigenvectors: All very similar, look at groups instead
pca$scores

##calculate Euclidean distance among sites
wtr.d<-round(var(scale(wtr,scale=F)),0)  #calculate variance-covariance matrix and save it to 'wtr.d'
e.1<-eigen(wtr.d) #eigen-analysis
pc.matrix.1<-(as.matrix(scale(wtr, scale=F)))%*%e.1$vectors #calculate pc.matrix
euc<-dist(scale(wtr, scale=F))  #calculate Euclidian distance among site for centered original data
round(euc,2)
euc.1<-dist(pc.matrix.1[,c(1,2)])  #calculate Euclidian distance among sites in PCA space using only first 2 PCs
round(euc.1,2)
plot(euc,euc.1,main="Shepard diagram", xlab="Distance in Multidimensional space", ylab="Distance in Reduced space")
#The plot shows the 


#-------
wq <- pnw[c(2:9, 11:20)] #water quality variables
ws <-pnw[c(21,23:24,26:32)] #watershed variables

#make sure to scale data
boxplot(scale(log(wq+1))) 
boxplot(scale(ws))


