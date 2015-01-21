###Week 2 PCA plots: More options
###import a data called 'usair1' with an additional categorical var
##Run PCA in R
library(MASS) #call the package of "MASS" to make 'princomp' function available
pca1<-princomp(scale(air1[,-c(1,9)],scale=F)) #run PCA on covariance and save all outputs in 'pca1'
pca1$scores  #print pc.matrix.  What is pc.matrix?
plot(pca1$scores[,1], pca1$scores[,2],xlab="PC 1", ylab="PC 2") # plot the first two PCs--boring!
plot(pca1$scores[,1], pca1$scores[,2],type='n',xlab="PC 1", ylab="PC 2") # plot the first two PCs--Frame only with no cities
text(pca1$scores[,1], pca1$scores[,2],labels=air1$City, lwd=2) #add city names to the plot
plot(pca1$scores[,1], pca1$scores[,2],type='n',xlab="PC 1", ylab="PC 2") # plot the first two PCs--Frame only 
text(pca1$scores[,1], pca1$scores[,2],labels=air1$Group, lwd=2) #add city population size (Group variable)
plot(pca1$scores[,1], pca1$scores[,2],type='n',xlab="PC 1", ylab="PC 2") # plot the first two PCs--Frame only 
symbols(pca1$scores[,1], pca1$scores[,2],circles=air1[,2], inches=0.2) #show relative concentrations of SO2 among cities
## one more
plot(pca1$scores[,1], pca1$scores[,2],pch=as.numeric(air1$Group),xlab="PC 1 (97.6%)", ylab="PC 2(2%)") #I add eigenvalues for each axis
legend(locator(1),c("Large","Small"),pch=c(1,2)) #click where you want to put the legend in the plot
