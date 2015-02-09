#Bern Romey 08Feb15, hw3rda portion.  Lecture 7 slide presentation

dta <- read.csv("wemap_pnw_rda_HW.csv")
pnw <- na.omit(dta) #remove missing data
rm(dta)
str(pnw)

#log transformed, centered to Z score
wq.l <-scale(log(pnw[c(2:9, 11:20)]+1)) #water quality variables
ws.l <-scale(log(pnw[c(21,23:24,26:32)]+1)) #watershed variables
boxplot(wq.l)
boxplot(ws.l)


#RDA
library(vegan)
mod<-rda(wq.l~ws.l, scale=T)
mod<-rda(wq.l~.,data=ws.l, scale=T)
mod
summary(mod)
plot(mod)
