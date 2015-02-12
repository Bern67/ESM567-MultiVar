#Bern Romey 08Feb15, hw3rda portion.  Lecture 7 slide presentation

dta <- read.csv("wemap_pnw_rda_HW.csv")
pnw <- na.omit(dta) #remove missing data
rm(dta)
str(pnw)

#Centered to zero with Z-score
wq <-scale(pnw[c(2:9, 11:20)]) #water quality variables
ws <-scale(pnw[c(21,23:24,26:32)])
boxplot(wq)
boxplot(ws)
#log transformed, centered to Z-score
wq.l <-scale(log(pnw[c(2:9, 11:20)]+1)) #water quality variables
ws.l <-scale(log(pnw[c(21,23:24,26:32)]+1)) #watershed variables
boxplot(wq.l)
boxplot(ws.l)


#RDA
library(vegan)
library(MASS)
mod<-rda(wq.l~ws.l, scale=T)
mod<-rda(wq.l~.,data=ws.l, scale=T)
mod
summary(mod)
plot(mod)



#1 RDA Full model
#How well two matrices are associated? (constrained inertia)
#Is the RDA model relationship between the two matrices significant?	(anova.cca: global permutation test)
#Can the RDA model be reduced?(step selection with AIC plus vif)
                  
#2 RDA reduced model
#How well two matrices are associated? (constrained inertia)
#Is the RDA model relationship between the two matrices significant?	(anova.cca: global permutation test)
#How many RDA axes are significant? (anova.cca: by “axis”)	
#How many RDA axes to interpret? (eigenvalues for each axis)
#What each RDA axis represents? (Biplot scores for constraining variables)
#What are spatial patterns/trends among sitesin the reduced RDA space? 
# Compare spatial patterns/trends among sites between PCA and RDA plots 
# Compare eigenvalues of RDA1 and PC1 
# Interpret the relationships between the two matrices using key results

#3 Variance partition with partial RDA:

#Can we group Xs into big categories (natural vs. anthropogenic)?
# Which category of Xs is more important than others (varpart) on Ys?
# What are their interactive effects on Ys?


