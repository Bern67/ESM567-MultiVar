###MANOVA and Linear Discriminant Function Analysis 
###Use the Florida Everglades marsh data (we used it for Homework#1: PCA)
###But I only include 8 nitrogen or phosphorus variables
###I used diatom assemblage data collected simultaneously with the water quality to classify all 32 sites into 3 groups
###I include this categorical variable in the dataset and save the data as 'everglade'
###The hypothesis: is there statistical difference in terms of nutrients among the 3 diatom-based groups



head(everglade)
x<-as.factor(everglade[,1]) #independent variable (factor)
y<-as.matrix(everglade[,-c(1,10)]) #multivariate response variable matrix
##Run PCA first
pca<-princomp(scale(y),scale=F)
summary(pca)
plot(pca$scores[,1],pca$scores[,2],xlab="PC I (36%)",ylab="PC II (18%)")
plot(pca$scores[,1],pca$scores[,2],type='n',xlab="PC I (36%)",ylab="PC II (18%)")
text(pca$scores[,1],pca$scores[,2],labels=everglade$Group)

##Run one-way MANOVA
mod<-manova(y~x)
summary.manova(mod) #MANOVA table, compare it with univariate ANOVA table
summary.manova(mod, test = "Pillai") #4 MANOVA statistics are calculated and you can choose
summary.manova(mod, test = "Wilks")
summary.manova(mod, test = "Hotelling-Lawley")
summary.manova(mod, test = "Roy") #with a large sample size, all 4 statistics should converge well

### check for the two most important assumptions for MANOVA
#1. Multi-Normality assumption
#Graphic assessment of multi-normality: chi-square plot written by Everitt
#it is similar to a q-q plot in univariate ANOVA
chisplot <- function(x) {
    if (!is.matrix(x)) stop("x is not a matrix")

    ### determine dimensions
    n <- nrow(x)
    p <- ncol(x)
    #
    xbar <- apply(x, 2, mean)
    S <- var(x)
    S <- solve(S)
    index <- (1:n)/(n+1)
    #
    xcent <- t(t(x) - xbar)
    di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
    #
    quant <- qchisq(index,p)
    plot(quant, sort(di), ylab = "Ordered distances",
         xlab = "Chi-square quantile", lwd=2,pch=1)
         return (di)

}

chisplot(resid(mod))  # the dataset must be in matrix format, original data

#run a normality test "mshapiro.test"(similar to "Shapiro.test") 
#but you need to install a package (mvnormtest) first
library(mvnormtest)
mshapiro.test(t(resid(mod))) #data has to be a matrix and has to be transposed

#check for individual variable's normality using Q-Q plot
#I used a simple loop to do this repetitive work
par(mfrow=c(2,4))
names<-colnames(y)
for (i in 1:ncol(y)){
  qqnorm(y[,i],main=paste("Q-Q plot of",names[i]))
  qqline(y[,i])
}

##refit manova with log-transformed response variables
mod.log<-manova(log(y+1)~x)

##test the normality assumption again
chisplot(resid(mod.log))
mshapiro.test(t(resid(mod.log)))

#refit manova exclusing 2 datapoints (7,29)
ever.30<-everglade[-c(7,29),]
x.30<-as.factor(ever.30[,1])
y.30<-as.matrix(ever.30[,c(2:9)])
mod.30<-manova(log(y.30+1)~x.30)

##test the normality assumption again
chisplot(resid(mod.30))
mshapiro.test(t(resid(mod.30)))  #small size issue!

par(mfrow=c(2,2))
chisplot(resid(mod))
chisplot(resid(mod.log))
chisplot(resid(mod.30))

### check for the two most important assumptions for MANOVA
#2. homogeneity of covariance matrices
#use a loop to make 8 boxplot to check if variance is equal among the groups
par(mfrow=c(2,4))
names<-colnames(y.30)
for (i in 1:ncol(y)){
  boxplot(y.30[,i]~x.30,main=paste("Boxplot of",names[i]),xlab="Groups", ylab=paste(names[i]))
}

#log-transform
par(mfrow=c(2,4))
names<-colnames(y.30)
for (i in 1:ncol(y)){
  boxplot(log(y.30[,i]+1)~x.30,main=paste("Boxplot of log",names[i]),xlab="Groups", ylab=paste("log",names[i]))
}

##test homogeneity of covariance matrices 
##the test is developed by Anderson and is similar to Levene's univariate test for equal variance 
library(vegan) #Required functions for testing are in the 'vegan' Package
m<-betadisper(dist(y.30),ever.30$GRP, type="centroid") #calculate average distance to its group centroid for each group
m.HSD<-TukeyHSD(m) #run TukeyHSD pair-wise test difference in dispersion between groups 
plot(m.HSD) #graphic display of pair-wise comparisons of difference in dispersion between groups

#repeat the same test with log-transformed data
m.Log<-betadisper(dist(log(y.30)+1),ever.30$GRP, type="centroid") #calculate average distance to its group centroid for each group
m.Log.HSD<-TukeyHSD(m.Log) #run TukeyHSD pair-wise test difference in dispersion between groups 
plot(m.Log.HSD) #graphic display of pair-wise comparisons of difference in dispersion between groups

###Perform linear Discriminant Function Analysis (DFA)
library(MASS) #"lda" function is in the MASS package
dfa<-lda(x.30~y.30) #run inear Discriminant Function Analysis
dfa #outputs, similar to PCA's output
plot(dfa, cex=1.5) #DFA plot (similar to a PCA plot)
tb<-table(Predicted=predict(dfa,ever.30[,-c(1,10)],type='class')$class,Observed=ever.30[,"Group"])
tb #show a confusion table (classification results using the DFA model)
((1-sum(diag(tb)/sum(tb)))*100) #mis-classification rate

library(klaR)
g<-greedy.wilks(scale(y),x)

