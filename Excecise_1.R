###Worksheet #1

library(dplyr)
library(ggplot2)

##import the datset 'wemap' to R and save it as 'wemap'
wemap <- read.csv("wemap_na.csv")

##Exercise 1. Use dplyr to manipulate and summarize the wemap dataset

#1.  Describe the data
#a.	How many variables and observations?
dim(wemap)  #20 variables and 1105 observations

#b.  How many variables are numerical and how many variables are factor/categorical?
str(wemap) #strictly speaking 2 variables are factors (ECO3 and ECO10), actuall YEAR and HUC4 should also be viewed as 'factor'

#c.	How many missing values (Na) in the dataset?
sum(is.na(wemap))  #13

#d.	How to remove all NAs from the dataset?
wemap.1<-na.omit(wemap)
sum(is.na(wemap.1))  #0

#e.	How many sites for each ecoregion (ECO3)?
table(wemap.1$ECO3) #MT:590, PL:190, XE: 222

#f.	How many sites for each ecoregion and each year?
table(wemap.1$ECO3, wemap.1$YEAR)
#   2000 2001 2002 2003 2004
MT  106  149  183   65   87
PL   47   39   60   19   25
XE   30   45   65   25   57

#2.  Create a dataset with all water quality variables only
colnames(wemap.1) #list all variables
wq<-select(wemap.1,ANC, CA, CL, COND, DOC, Sodium,NH4,NO3,NTL,PH,SIO2,TSS,TURB)
head*wq)

#or
wq.1<-select(wemap.1,-c(YEAR,ECO10,ECO3,HUC4,AG_TOT,FOR_TOT,URB_TOT))
head(wq.1)

#3.  Order the above dataset by a variable of AG_TOT 
wq.ag<-arrange(wq.1,wemap.1$AG_TOT)
head(wq.ag)

#4.  Create a new variable (log COND) by transforming COND using a log function
wq.2<-mutate(wq.1, logCOND=log(COND))
head(wq.2)

#5.  Create a dataset with all water quality variables only for one of the ecoregions (PL in ECO3) AND the sites with PH>7
wq.3<-filter(wq.2, wemap.1$ECO3=='PL'& PH>7)
summary(wq.3$PH) #check if all sites with PH>7

#6.  Calculate mean and variance of COND and log COND for each ecoregion (ECO3)
by_ECO3<-group_by(wq.2,wemap.1$ECO3)
summarise(by_ECO3,mean_COND=mean(COND), 
          mean_logCOND=mean(wq.2$logCOND),
          var_COND=var(COND), 
          var_logCOND=var(wq.2$logCOND))


##Exercise 2. Use ggplot2 to display the data
#1.  Describe the distribution of COND using ggplot2
library(ggplot2)
d<-ggplot(wemap.1,aes(COND))
d+geom_histogram()

#2.  Describe the distribution of COND using ggplot2 for each ecoregion (ECO3)
d+geom_histogram()+facet_grid(ECO3~.)
d+geom_histogram()+facet_grid(~ECO3) #how does it differ from the above one

#3.	Compare mean and 95% confidence intervals of COND among the 3 ecoregions (ECO3)
d1<-ggplot(wemap.1,aes(y=COND, x=ECO3))
d1+stat_summary(fun.data="mean_cl_normal", geom="errorbar")+stat_summary(fun.y=mean, geom="point", size=I(5))

#4.	Make a x-y scatter plot between COND and ANC.
d2<-ggplot(wemap.1,aes(y=COND, x=ANC))
d2+geom_point()

#5.	Make a x-y scatter plot between log-transformed COND and log-transformed ANC.  How does the relationship change and why?
d3<-ggplot(wemap.1, aes(y=log(COND),x=log(ANC+1)))
d3+geom_point()
#or you can do
d2+geom_point(aes(y=log(COND), x=log(ANC+1)))+xlab("Log ANC")+ylab("Conductivity")

#6.	Make a x-y scatter plot between log-transformed COND and log-transformed ANC for each ecoregion.  How does the relationship differ among the ecoregions?
d3+geom_point()+xlab("Log ANC")+ylab("Conductivity")+facet_grid(ECO3~.)
d3+geom_point()+xlab("Log ANC")+ylab("Conductivity")+facet_grid(ECO3~.)+geom_smooth(aes(group=ECO3), method='lm')


