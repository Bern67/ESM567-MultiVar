##Import data called 'wemap' and learn more about the data
<<<<<<< HEAD
wemap <-read.csv("wemap.csv")

dim(wemap)
=======
dta <- read.csv("wemap_na.csv")

>>>>>>> f96bf6dd6fedf45912bab0820802aae3347b306a
colnames(wemap) #show all variables
head(wemap) #show the first 6 rows of the data
tail(wemap) #show the last 6 rows of the data
dim(wemap)  #show the dimension of the data (the number of rows and columns)
str(wemap)  #show the structure of the data
summary(wemap)  #summarize the data
mean(wemap$COND) #mean of a variable called "COND"


########################################################################################
##Graphics using 'ggplot2' package
install.packages('ggplot2')
library(ggplot2) #graphic package

## One signle variable to view its distribution
d<-ggplot(wemap,aes(PH))+theme(text=element_text(size=20)) # set up data and mapping plus font size (20 for all text)
d+geom_histogram() #add a layer (geom_histogram) to make a histogram
d+geom_histogram(binwidth=0.5)  #change the parameter for geom_histogram
d+geom_histogram()+stat_bin(binwidth=0.5) #the same as the above line using stat
d+geom_histogram(aes(fill=ECO3)) #change histogram's aesthetic attributes
d+geom_bar(aes(fill=ECO3)) #an alternative way to do the same as the above
d+geom_density() #make a density plot instead
d+geom_density(aes(color=ECO3))  ##change density plot's aesthetic attributes to clearly show 3 ecoregions

dd<-ggplot(wemap,aes(PH, fill=ECO3))+theme(text=element_text(size=20)) 
dd + geom_bar(position = "stack")
dd + geom_bar(position = "fill")
dd + geom_bar(position = "dodge")

##one numerical variable vs one categorical variable
d1<-ggplot(wemap,aes(y=PH, x=ECO3))+theme(text=element_text(size=20))
d1+geom_boxplot()
d1+geom_boxplot()+geom_point()
d1+geom_boxplot()+stat_summary(fun.y=mean, geom="point", color='red', size=5)
d1+stat_summary(fun.data="mean_cl_normal", geom="errorbar", width=0.2)+stat_summary(fun.y=mean, geom="point", size=I(5))  # mean and 95% confidence intervals for each ecoregion

##one numerical variable vs another numerical variable

p<-ggplot(wemap,aes(PH, log(ANC+1)))
p+geom_point()+theme_bw(20)  
p+geom_point()+geom_smooth(method='lm') 
p+geom_point(aes(color=ECO3)) 
p+geom_point(aes(color=ECO3))+geom_smooth(aes(group=ECO3),method='lm', se=F)
p+geom_point(aes(color=ECO3))+geom_smooth(aes(group=ECO3),method='lm')
p+geom_point(aes(shape=ECO3,color=ECO3))+geom_smooth(aes(group = ECO3), method="lm")
p+geom_point()+facet_grid(ECO3~.)+theme_bw()+geom_smooth(method='lm', color='red') 
p+geom_point()+facet_grid(ECO3~YEAR)+theme_bw()+geom_smooth(method='lm', color='red') 


##publish plots: create 3 separate plots a,b, and c
a<-p+geom_point()+geom_smooth(method='lm') 
b<-d+geom_histogram() 
c<-d1+geom_boxplot()+stat_summary(fun.y=mean, geom="point", color='red', size=5)

##create a pdf 
pdf("subplot-1.pdf", width = 4, height = 4) #save the figure as a pdf with a file name of subplot-1.pdf
subvp <- viewport(width = 0.3, height = 0.3, x = 0.8, y = 0.32) #define the dimension and location of the subplot
a  #main plot
print(b, vp = subvp)  #insert a subplot
dev.off()

csmall <- b +   #clean-up the subplot by removing axis labels, changing plot margin and reducing font size
  theme_gray(9) + 
  labs(x = NULL, y = NULL) + 
  opts(plot.margin = unit(rep(0, 4), "lines"))

pdf("subplot-2.pdf", width = 4, height = 4)  #save the figure as a pdf with a file name of subplot-2.pdf
a
print(csmall, vp = subvp)  ##insert the revised subplot
dev.off()

pdf("layout.pdf", width = 8, height = 6)  #save the figure as a pdf with a file name of 'layout.pdf'
grid.newpage()  #move to a new page on a grid device
pushViewport(viewport(layout = grid.layout(2, 2))) #set up a grid layout with 2X2 multiple panels per page

vplayout <- function(x, y) 
  viewport(layout.pos.row = x, layout.pos.col = y)  # create a function to set up a layout for multiple figures
print(a, vp = vplayout(1, 1:2))  #add plot a (take the 1st row position but strech to both columns)
print(b, vp = vplayout(2, 1))  # add plot b (take the 2nd row position and 1st column)
print(c, vp = vplayout(2, 2))  # add plot c (take the 2nd row position and 2nd column)
dev.off()

####################################################################################################
###### using 'qplot' to make similar graphes
qplot(PH,ANC,data=wemap)  #simple plot
qplot(PH,ANC,data=wemap, size=I(2), colour=I('red'))
qplot(PH,ANC,data=wemap, colour=YEAR) #add the 3rd variable (year) with differen colors
qplot(PH,ANC,data=wemap,size=AG_TOT) #add the 3rd variable (AG_TOT) to make a bubble plot
qplot(PH,ANC,data=wemap, shape=ECO3, size=I(3), colour=I('red')) ##add the 3rd variable (ECO3) with different symbols

qplot(PH,ANC,data=wemap,geom=c("point","smooth")) #curve fitting
qplot(PH,ANC,data=wemap,geom=c("point","smooth"),method="lm") #fit a linear line

qplot(ECO3,ANC,data=wemap,geom="boxplot") #boxplot
qplot(ANC,data=wemap,geom="histogram") #bar/histogram
qplot(ANC,data=wemap,geom="histogram",fill=ECO3)  #show the 3rd variable
qplot(ANC,data=wemap,geom="density")
qplot(ANC,data=wemap,geom="density", colour=ECO3)

qplot(PH,ANC,data=wemap)+facet_grid(.~ECO3)  #by ECO3
qplot(PH,ANC,data=wemap)+facet_grid(YEAR~ECO3)  #by ECO3 and YEAR
qplot(ECO3,ANC,data=wemap,geom="boxplot")+facet_grid(YEAR~.) #boxplot by YEAR
qplot(ECO3,ANC,data=wemap,geom="boxplot")+facet_grid(YEAR~ECO3)


############################################################################################
############################################################################################







install.packages(c("plyr", "reshape")) #Install two new R packages for data manipulations
library(plyr) #activate the data manipulation package

##Using 'plyr' R pacakge to manipulate the data
ddply(wemap,"ECO3",summarize,Average=mean(COND)) #calculate mean of conductivity for each Ecoregion
ddply(wemap,"ECO3",summarize,Average=mean(COND),Stdev=sd(COND)) ##calculate mean and standard deviation of conductivity for each Ecoregion
D<-ddply(wemap,"ECO3",function(x){
  SD<-sd(x$COND)
  N<-length(x$COND)
  SE<-SD/sqrt(N)
}) #using a simple function to calculate standard error of the mean of conductivity for each ecoregion
names(D)[2]<-"SE" #change the 2nd column's name from "V1" to "SE"
ddply(wemap,c("ECO3","YEAR"),summarize,Conductivity=mean(COND)) #calculate mean of conductivity for each ecoregion and each year
ddply(wemap,"ECO3",function(df)colMeans(df[,6:8])) #calculate means of multiple variables for each ecoregion
ddply(wemap,"ECO3",colwise(mean)) #calculate means of ALL variables for each ecoregion
ddply(wemap,"ECO3",numcolwise(mean)) #calculate means of ALL Numerical variables for each ecoregion
ddply(wemap,c("ECO3","YEAR"),numcolwise(mean)) #calculate means of ALL numerical variables for each ecoregion and year

S<-ddply(wemap,"ECO3",transform, logCond=log(COND+1)) #using 'transform' to add a new variable (log-transformed Conductivity)

ddply(wemap,"ECO3",subset,AG_TOT==max(AG_TOT)) #using 'subset' to select a portion of the data: the sites with maximal % of agriculture in each group
ddply(wemap,"ECO3",subset,AG_TOT>60) #in each ecoregion, select the sites with % agriculture>60%
ddply(wemap,"ECO3",subset,AG_TOT>quantile(AG_TOT,0.99)) #in each ecoregion, select the sites with 1% highest agriculture in the watershed

##Using 'reshape' R pacakge to manipulate the data
library(reshape) #data manipulation package 
dta<-melt(wemap[,c(1:10)],id=c("YEAR","ECO10","ECO3","HUC4")) #melt data
cast(dta,variable~., length) #sample size for each variable 
cast(dta,variable~., c(median,min,max)) #median, minimum, and maximum for each variable
cast(dta,variable~., summary) #summary statisitcs for each variable
cast(dta,YEAR~variable, length) #sample size for each variable by year
cast(dta,YEAR+ECO3~variable, length) #sample size for each variable by year and ecoregion
cast(dta,YEAR~variable, sum) #reshape the melt data with aggregation (summation of each variable by year)
cast(dta,YEAR~variable, mean) #reshape the melt data with aggregation (mean of each variable by year)
cast(dta,YEAR~variable, mean,margins=TRUE) #show both row and column means
cast(dta,YEAR~variable, mean,margins="grand_col") #show mean for each column
cast(dta,YEAR~variable, summary) #with summary statistics
cast(dta,YEAR~variable, c(median,min,max)) #with multiple functions


