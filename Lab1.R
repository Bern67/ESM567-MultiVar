#Lab 1
library(dplyr)
dta <-tbl_df(read.csv("wemap_na.csv"))
dta
#How many variables and observations?
dim(dta)
#How many variables are numerical vs catagorical?
str(dta)
#How many missing values (na)?
summary(dta)
#or
sum(is.na(dta))

#remove na's
dta.1<-na.omit(dta)
summary(dta.1)

#How many sites for each ecoregion (ECO3)?
library(plyr)
count(dta.1$ECO3)
table(dta.1$ECO3)

#How many sites for each ecoregion and each year?
table(dta.1$ECO3, dta.1$YEAR)

#Create data set with only water quality data
library(dplyr)
wq <- select(dta.1,-c(YEAR,ECO10,ECO3,HUC4,AG_TOT,FOR_TOT,URB_TOT))
wq
#Order the above dataset by AG_TOT
wq.ag<-arrange(wq,dta.1$AG_TOT)

#Ctreate new variable (log_COND) by transforming variable COND
wq.1 <- mutate(wq, logCOND =log(COND)) #using dplyr
wq.1

#Create new dataset for ecoregion (PL in ECO3) and the site with pH>7
wq.2<-filter(wq, dta.1$ECO3=='PL'& PH>7)
summary(wq.2$PH)

#Calcualte the mean and var of COND and log_COND for each echoregion (ECO3)

by_ECO3<-group_by(wq.1,dta.1$ECO3)
summarise(by_ECO3,mean_COND=mean(COND), 
          mean_logCOND=mean(wq.1$logCOND),
          varCOND=var(COND), 
          var_logCOND=var(wq.1$logCOND))

##Exercise 2. Use ggplot2 to display the data
#1.  Describe the distribution of COND using ggplot2
library(ggplot2)
d<-ggplot(dta.1,aes(COND))
d+geom_histogram()

#2.  Describe the distribution of COND using ggplot2 for each ecoregion (ECO3)
d+geom_histogram()+facet_grid(ECO3~.)
d+geom_histogram()+facet_grid(~ECO3) #how does it differ from the above one

#3.  Compare mean and 95% confidence intervals of COND among the 3 ecoregions (ECO3)
d1<-ggplot(dta.1,aes(y=COND, x=ECO3))
d1+stat_summary(fun.data="mean_cl_normal", geom="errorbar")+stat_summary(fun.y=mean, geom="point", size=I(5))

#4.	Make a x-y scatter plot between COND and ANC.
d2<-ggplot(dta.1,aes(y=COND, x=ANC))
d2+geom_point()

#5.	Make a x-y scatter plot between log-transformed COND and log-transformed ANC.  How does the relationship change and why?
d3<-ggplot(dta.1, aes(y=log(COND),x=log(ANC+1)))
d3+geom_point()
#or you can do
d2+geom_point(aes(y=log(COND), x=log(ANC+1)))+xlab("Log ANC")+ylab("Conductivity")

#6.	Make a x-y scatter plot between log-transformed COND and log-transformed ANC for each ecoregion.  How does the relationship differ among the ecoregions?
d3+geom_point()+xlab("Log ANC")+ylab("Conductivity")+facet_grid(ECO3~.)
d3+geom_point()+xlab("Log ANC")+ylab("Conductivity")+facet_grid(ECO3~.)+geom_smooth(aes(group=ECO3), method='lm')

