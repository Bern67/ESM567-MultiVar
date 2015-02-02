###Week 2 Principal Component Analysis
###Before you run PCA, know your data graphically and numerically. Use the R scripts from Week 1 to summarize each variable numerically (e.g., mean, variance) and graphically (e.g., histogram, boxplot) and then check the relationships among variables (e.g., co-variance, correlation, cor.matrix)

air<-read.csv("usair1.csv", header=T)  

air.s<-air[,c(3:5)] #make a subset only 3 columns (variables)
air.s #look at the newly created dataset
var(air.s) #calculate variance and covariance matrix
boxplot(air.s) #boxplot
var(scale(air.s)) #calculate variance and covariance matrix with the standardized data
boxplot(scale(air.s)) #boxplot with the standardized data
##source cor.matrix in the way that you did last week
cor.matrix(air.s)


##Run PCA in R
library(MASS) #call the package of "MASS" to make 'princomp' function available
pca1<-princomp(scale(air.s,scale=F)) #run PCA on covariance and save all outputs in 'pca1'
########Think about the following questions ###################################################################################################
###How many variables in 'air.s'? How many PC components will be generated for this dataset? How do you rank the importance of each PC component?
###If you decide to use only one PC to 'represent' the original data, what much variance of the original data is captured by the PC component?
###How do you interpret what this PC component means in relation to all original variables?
###############################################################################################################################################

summary(pca1) #check all eigenvalues to determine components to use
loadings(pca1) # check all eigenvectors
names(pca1) #check what detailed PCA outputs are available
pca1$scores  #print pc.matrix.  What is pc.matrix?
plot(pca1$scores[,1], pca1$scores[,2],xlab="PC 1", ylab="PC 2", type='n') # plot the first two PCs.  Can you interpret the plot?
text(pca1$scores[,1], pca1$scores[,2],labels=air$City, lwd=2)
biplot(pca1) #show both sites and variables. Can you interpret the plot?
plot(pca1, main="Scree plot") #scree plot. What does this plot tell you?
##Try to run the same PCA with all variables and all cities##

#######################################################################
##STOP HERE! Wait for Wednesday's lecture for the following exercise ##
#######################################################################
###step-by-step run PCA using spectral decomposition method based on covariance matrix

air.s<-air[,c(3:5)] #make a subset with the first 5 rows (cities) and 4 columns (variables)
air.s #look at the newly created dataset
air.d<-round(var(scale(air.s,scale=F)),0)  #calculate variance-covariance matrix and save it to 'air.d'
air.d #look at the newly created variance-covariance matrix
e.1<-eigen(air.d) #eigen-analysis
names(e.1) #check how many R objects in "e" -->two: "values" and "vectors"
e1.value<-round(e.1$values,0) #get all eigenvalues
e1.value  #print out all eigenvalues
e1.vector<-round(e.1$vectors,2) #get all eigenvectors
e1.vector   #print out all all eigenvectors, Note: each eigenvector needs to be normalized (each number divides by the vector length)
e1.value/sum(e1.value) #get proportion of variance explained by each axis
sum(e1.value) # sum all eigenvalues
sum(diag(air.d))  #sum all variances
pc.matrix.1<-(as.matrix(scale(air.s, scale=F)))%*%e.1$vectors #calculate pc.matrix, need to convert the scaled original data into matrix first, "%*%" is for matrix multiplication
round(pc.matrix.1,2) #print pc.matrix
par(mfrow=c(1,1))
plot(pc.matrix.1[,1],pc.matrix.1[,2],xlab="PC 1", ylab="PC 2", cex=3)  # plot the first two PCs

###step-by-step run PCA using spectral decomposition method based on correlation matrix
air.c<-round(cov(scale(air.s)),2) #calculate a correlation matrix
e<-eigen(air.c) #eigen-analysis
names(e) #check how many R objects in "e" -->two: "values" and "vectors"
e.value<-round(e$values,2) #get all eigenvalues
e.value  #print out all all eigenvalues
e.vector<-round(e$vectors,2) #get all eigenvectors
e.vector   #print out all all eigenvalues
e.value/sum(e.value) #calculate what proportion of variance explained by each PC
pc.matrix<-(as.matrix(scale(air.s)))%*%e$vectors #calculate pc.matrix, need to convert the scaled original data into matrix first, "%*%" is for matrix multiplication
round(pc.matrix,2) #print pc.matrix
par(mfrow=c(1,1))
plot(pc.matrix[,1],pc.matrix[,2],xlab="PC 1", ylab="PC 2", cex=3)  # plot the first two PCs

### run the same analysis using a build-in function
library(MASS) #call the package of "MASS" to make 'princomp' function available
pca2<-princomp(scale(air.s)) #run PCA on correlation and save all outputs in 'pca1'
summary(pca2) #eigenvalues
loadings(pca2) # eigenvectors
names(pca2) #what components are in detailed PCA output
pca2$scores  #print pc.matrix
plot(pca2$scores[,1], pca2$scores[,2],xlab="PC 1", ylab="PC 2", cex=3) # plot the first two PCs
par(mfrow=c(1,1))
biplot(pca2) #show both sites and variables

##scree plot
par(mfrow=c(2,2))
plot(pca2)#scree plot
plot(pca1) #Explain the difference between the two scree plots.


##calculate Euclidean distance among sites
euc<-dist(scale(air.s, scale=F))  #calculate Euclidian distance among site for centered original data
round(euc,2)
euc.1<-dist(pc.matrix.1[,c(1,2)])  #calculate Euclidian distance among sites in PCA space using only first 2 PCs
round(euc.1,2)
plot(euc,euc.1,main="Shepard diagram", xlab="Distance in Multidimensional space", ylab="Distance in Reduced space")

##How many PCs should be selected--Broken Stick Model
##copy'broken.stick model' from D2L(week 2), paste it to new R file, and then save as 'broken.stick.R'
##source it using 'Source' button next to "Run" botton
broken.stick(3) #run Broken.stick model on a dataset with 3 variables
##out put
j      E(j)
[1,] 1 0.6111111  #Expected first eigenvalue by random is 0.61, if the 1st eigenvalue from your data>0.61, keep it
[2,] 2 0.2777778  #Repeat the above until the eigenvalues from your data is < the eigenvalues generated by random
[3,] 3 0.1111111
