#Bern Romey 08Feb15, hw3rda portion.  Lecture 6 & 7 slide presentation

#Data
#----
dta <- read.csv("wemap_pnw_rda_HW.csv")
pnw <- na.omit(dta) #remove missing data
rm(dta)
str(pnw)

#Centered to zero with Z-score
wq <-pnw[c(2:20)] #water quality variables (unconstrained)
ws <-pnw[c(21:34)]#watershed variables (constrained)
boxplot(wq)
boxplot(ws)
#log transformed, centered to Z-score
wq.l <-log(wq+1) #water quality variables (dependant/response)
ws.l <-log(ws+1) #watershed variables (independant/explanatory)
boxplot(scale(wq.l))
boxplot(scale(ws.l))

cor.matrix(wq.l)
cor.matrix(ws.l)


#RDA
#----
library(vegan)
library(MASS)
mod<-rda(wq.l~.,data=ws.l, scale=T) #Full RDA model

plot(mod, type="n") #triplot
points(mod, pch=21, col="darkgreen", bg="lightgrey", cex=1.2)
text(mod, dis="cn", col="red")
text(mod, "species", col="black", cex=0.8)
#water quality response variable(Y=black) letters, watershed explanatory variable are (X=red) vectors, and sites are green/grey dots.


#1 RDA Full model
mod #Inertia is variance (Lec 6)
#How well two matrices are associated? (constrained inertia)

summary(mod) #Species scores is eigenvectors

#Is the RDA model relationship between the two matrices significant?	(anova.cca: global permutation test)
#Can the RDA model be reduced?(step selection with AIC plus VIF)
anova.cca(mod) #yes, p-value = 0.001                 

#----

#Can the RDA model be reduced?
vif.cca(mod)#Redundancy among species (Veriance Inflation Factor)
# VIF > 4 or 5 suggests multi-collinearity; VIF > 10 is strong evidence 
#that collinearity is affecting the regression coefficients.

#Selection procedure (AIC approach)- Hybrid approach, search method that compares models sequentially

#Full model (with all Xs):
rda.ws<-rda(wq.l ~.,data=ws.l,scale=T)
#Null model (with no Xs):
rda.0<-rda(wq.l ~1, data=ws.l, scale=T)
#Hybrid selection:
rda.1<-step(rda.0, scope=formula(rda.ws))

#----

#Run VIF again on new selection.  See if there are any multi-col > 5
#If yes, regress and drop higher ones that are correlated
ws.l1 <-ws.l[,-c(2,5)]
rda.1<-rda(wq.l ~.,data=ws.l1,scale=T)
vif.cca(rda.1) #All multi-col < 5

#2 RDA reduced model
#How well two matrices are associated? (constrained inertia)
rda.1 #Inertia is variance (Lec 6)
#How well two matrices are associated? (constrained inertia)
summary(rda.1)#Species scores is eigenvectors


#Is the reduced RDA model relationship between the two matrices significant?  (anova.cca: global permutation test)
anova.cca(rda.1, step=1000)

#How many RDA axes are significant? (anova.cca: by “axis”)
anova.cca(rda.ws,by='axis', step=1000)

plot(rda.1, type="n") #triplot
points(rda.1, pch=21, col="darkgreen", bg="lightgrey", cex=1.2)
text(rda.1, dis="cn", col="red")
text(rda.1, "species", col="black", cex=0.8)

round(coef(rda.1)[,c(1:2)],3)

