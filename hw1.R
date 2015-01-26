# Hw1 script

dta <-read.csv("wemap.csv")
library(dplyr)
dta1 <-select(dta, -c(YEAR, ECO10, ECO3, HUC4))
var1<-var<-dta1%>%summarise_each(funs(var))  # variance for each variable in dta1
var1

#1
dta <-read.csv("wemap.csv")
library(dplyr)
dta<-select(dta,-c(YEAR, ECO10, ECO3, HUC4, FOR_TOT, AG_TOT, URB_TOT))

mean <- summarise_each(dta,funs(mean))
mdn<-summarise_each(dta,funs(median))
var <- summarise_each(dta,funs(var))
sd <- summarise_each(dta, funs(sd))
mx<-summarise_each(dta,funs(max))
mn<-summarise_each(dta,funs(min))

dta_2<- bind_rows(mean, mdn, var, sd, mx, mn)     
dta_2$new.col <- c("Mean","Median","Var","SD","Max","Min")
dta_stat<-rename(dta_2,stat=new.col)
dta_stat <- subset(dta_stat, select=c(stat,1:16))

# Transpose df
n <- dta_stat$stat # first remember the column names
dta.T <- as.data.frame(t(dta_stat[,-1])) # transpose all but the first column (name)
colnames(dta.T) <- n
dta.T$myfactor <- factor(row.names(dta.T))
dta.T <-arrange(dta.T,desc(Var))
dta.T<-rename(dta.T,Factor=myfactor)
dta.T<- subset(dta.T, select=c(Factor,1:6))
dta.T[,-1] <-round(dta.T[,-1],1) #the "-1" excludes column 1

str(dta.T) # Check the column types


#How many sites have % forest cover in watersheds greater than 80% in each ecoregion (ECO3)?
dta <-read.csv("wemap.csv")
library(dplyr)
by_ECO3<- dta %>%
  group_by(ECO3)%>%
  filter(FOR_TOT >80)%>%
  summarise(Number_site = n())

eco3<-dta%>%
  group_by(ECO3)%>%
  filter(FOR_TOT >80)%>%
  table(COND, NTL)

table(dta$COND, dta$NTL)

#Plots

dta <-read.csv("wemap.csv")
require(ggplot2)
p <- ggplot(dta, aes(factor(ECO3), COND))
p + geom_boxplot()+geom_point()

p1 <- ggplot(dta,aes(y=COND, x=ECO3))+theme(text=element_text(size=20))
p1 + stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.2) + stat_summary(fun.y=mean, geom="point", size=I(5))  # mean and 95% confidence intervals for each ecoregion

library(dplyr)
m1<-dta%>% # log transform TSS and TURB, then include in new df m1
  mutate(lg_TSS=log(TSS), 
  lg_TURB=log(TURB))

         
# scatterplots with linear model line and CI      
p<-ggplot(dta,aes(log(TSS+.25), log(TURB)))
p+geom_point()+theme_bw(20)  
p+geom_point()+geom_smooth(method='lm') 
p+geom_point(aes(color=ECO3)) 
p+geom_point(aes(color=ECO3))+geom_smooth(aes(group=ECO3),method='lm', se=F)
p+geom_point(aes(color=ECO3))+geom_smooth(aes(group=ECO3),method='lm')
p+geom_point(aes(shape=ECO3,color=ECO3))+geom_smooth(aes(group = ECO3), method="lm")
p+geom_point()+facet_grid(ECO3~.)+theme_bw()+geom_smooth(method='lm', color='red') 
p+geom_point()+facet_grid(ECO3~YEAR)+theme_bw()+geom_smooth(method='lm', color='red') 
