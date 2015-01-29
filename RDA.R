##### PCA with indirect analysis and compare it with RDA
##### USEPA's stream water quality data from California
##### Import a data called"wemap_cal.csv" and save it as 'ca'
ca <-read.csv("wemap_cal.csv")

colnames(ca)
ca.1<-na.omit(ca)  #remove all missing values from the dataset
wq<-ca.1[,c(2,4:6,12:15,19,22,24,26:27)]      #split the data into a subset with all water quality variables
ws<-ca.1[,-c(2,4:6,12:16,19,22,24,25,26:27)]    #split the data into a subset with all watershed variables

#####know your data using several graphic methods
boxplot(wq)
boxplot(ws)

#source "cor.matrix.r"
cor.matrix(wq[,c(1:5)]) #only show 5 variables to check each variable's distribution and pair-wise relationships
cor.matrix(log(wq[,c(1:5)]+1))  #log-transformation: How does transformation change both distribution and pair-wise relationships?
round(var(log(wq+1)),2) #variance/covariance matrix
diag(round(var(log(wq+1)),2)) #show variance only

#####run PCA first
pca.wq<-princomp(scale(log(wq+1))) #run PCA with log-transformed and standardized data
biplot(pca.wq) #biplot
summary(pca.wq) #eigenvalues
round(loadings(pca.wq)[,c(1:2)],2)  #eigenvectors for PC1 and 2 only
#source "brokenStick.r"
broken.stick(13) #how many PCs should we keep?
pca.wq$scores #PC.Matrix showing site scores for all PCs
###Perform an Indirect analysis: regress PC1 against 4 watershed variables
dta.new<-data.frame(PCI=pca.wq$scores[,1],ws)  #merge two datasets into one side by side
colnames(dta.new)
cor.matrix(dta.new[,c(1,2,10)])   #only show 2 explanatory variables
mod<-lm(PCI~scale(log(AG_TOT+1))+scale(log(PRECIP_M)+1), data=dta.new) #simple regression
summary(mod) #detailed regression model output, How much variance in PC1 is explained by the two variables?

###run RDA
library(vegan)    #call for 'vegan' package with redundancy analysis function
rda.ca<-rda(log(wq+1)~.,data=ws,scale=T)     #run RDA with standardized data and save everything in a R object (rda.ca)
plot(rda.ca) #triplot
summary(rda.ca) #detailed RDA output and use the key results to interpret the triplot
round(vif.cca(rda.ca),2)    #calculate Variance inflation factor for the RDA model to check multi-colliearity issue
rda.ca.0<-rda(log(wq+1)~1, data=ws,scale=T) #a null model with no explanatory variables at all
rda.ca.1<-step(rda.ca.0, scope=formula(rda.ca))  #step-wise selection using AIC
plot(rda.ca.1) #compare it with the previous triplot
vif.cca(rda.ca.1) #check the new model for multi-colliearity issue, which variable has VIF>10
cor(ws) #check which variable is highly correlated with the variable with VIF>10
par(mfrow=c(1,2))
plot(ws$FOR_TOT~ws$RNG_TOT, cex=2,ylab="% Forest",xlab="% Range land")
plot(ws$LAT_DD~ws$LON_DD, cex=2,ylab="Lat",xlab="Lon")
rda.ca.2<-rda(log(wq+1)~., data=ws[,-c(2,7,8,11,13,14,15)], scale=T) #A new model with 7 variables removed
vif.cca(rda.ca.2) #check the new model for multi-colliearity issue, which variable has VIF>10
##rda.ca.2 is the final model
plot (rda.ca.2) 
summary(rda.ca.2)
anova.cca(rda.ca.2,step=1000)   #Overall RDA model significant test
anova.cca(rda.ca.2,by="axis", step=1000) #check which RDA axis is signficant
#####

##partial RDA: how much variance in water quality is exclusively due to %ag, precipitation, or their interactions
rda.ca.3<-rda(log(wq+1)~AG_TOT+PRECIP_M,data=ws,scale=T)  #rda with two explanatory variables
summary(rda.ca.3) #28% variance explained by %Ag and Precipitation
rda.ag<-rda(log(wq+1)~AG_TOT,data=ws,scale=T) #rda with %ag only
summary(rda.ag)   #18% variance in wq can be explained by %ag
rda.ag.c<-rda(log(wq+1)~AG_TOT+Condition(PRECIP_M),data=ws,scale=T) #rda with %ag after "partial-out" the precipitation effect
summary(rda.ag.c)  #after partial-out precip, ag only explains~11%.
rda.nat.c<-rda(log(wq+1)~PRECIP_M+Condition(AG_TOT),data=ws,scale=T)
summary(rda.nat.c) #after partial-out %ag, precip only explains~10%.
##how much varaince in water quality variables is explained by the interactive effects of both %ag and precip?


##variance partitioning: an easy way using a built-in function in vegan called "varpart")
##very useful way to seperate anthropogenic effects from natural effects on water quality
par.2<-varpart(log(wq+1),ws[,1], ws[,9], scale=T)
par.2
plot(par.2)

par.6<-varpart(log(wq+1),ws[,c(1,4,10)], ws[,c(3,6,9)], scale=T)
par.6
plot(par.6)

showvarparts(2)
