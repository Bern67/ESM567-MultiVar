###Import the data called 'sp_e.csv', diaton species data I collected from Florida Everglades and save it as 'sp'
sp <- read.csv("sp_e.csv")

dim(sp)
#run mds in R
library(vegan)
library(MASS)
spp<-sqrt(sp) #transform spp to dampen importance of dominant taxa
mds2<-metaMDS(spp,k=2,trace=T,autotransform =F) #MDS with 2 dimension
spp.dist<-vegdist(spp)  #calculate Bray-Curtis distance among all sites
stressplot(mds2,spp.dist)  #generate a Sharperd diagram by plotting BC between sites against Eucledian distance between sites in NMDS 
mds2$stress #check for NMDS' stress value
par(mfrow=c(2,2)) #make a few bubble plots to find out how some species 'shape-up' the NMDS configration
plot(mds2$points[,1],mds2$points[,2],type="n", xlab="NMDS I", ylab="NMDS II")
symbols(mds2$points[,1],mds2$points[,2],circles=spp$NITFRUST, inches=0.4, add=T, lwd=2)
plot(mds2$points[,1],mds2$points[,2],type="n", xlab="NMDS I", ylab="NMDS II")
symbols(mds2$points[,1],mds2$points[,2],circles=spp$EUNPECTI, inches=0.4, add=T, lwd=2)
plot(mds2$points[,1],mds2$points[,2],type="n", xlab="NMDS I", ylab="NMDS II")
symbols(mds2$points[,1],mds2$points[,2],circles=spp$CYMMICRO, inches=0.4, add=T, lwd=2)
plot(mds2$points[,1],mds2$points[,2],type="n", xlab="NMDS I", ylab="NMDS II")
symbols(mds2$points[,1],mds2$points[,2],circles=spp$COCPLACE, inches=0.4, add=T, lwd=2)
text(mds2)

#make a few plots to compare how well each ordination (NMDS, CA, and PCA) is doing
par(mfrow=c(2,2))
sites<-read.csv("map.csv") #import a dataset with lat and long for each site
plot((sites$Latitude*(-1)),(sites$Longitude*(-1)), type="n")   #show each site by its own spatial location (I altered spatial orientation so that we can compare it with ordiation plots easily
text((sites$Latitude*(-1)),(sites$Longitude*(-1)))
plot(mds2$points[,1],mds2$points[,2],type="n",xlab="NMDS I", ylab="NMDS II")  #NMDS plot
text(mds2$points[,1],mds2$points[,2])

ca<-cca(spp)      #Correspondence analysis
plot(ca,display="sites")

pca<-rda(spp)    #Principal component analysis
plot(pca,display="sites")


##The following is a R script written by Chris Parker##
###to run the above functions, submit all lines to R first, then run each function with your data, eg. NMDS(sp)##
#######################################################################
#######################################################################
#######################################################################
NMDS<-function(x,method="bray",k=2)
{
mds<-metaMDS(x,autotransform=F,distance=method,k=k)
dis<-vegdist(x,method=method)
pts<-mds$points
rst<-apply(pts,2,function(column){
 y.dist<-dist(column)
 cor(dis,y.dist)^2
})

ydis<-dist(pts)
ttl<-cor(dis,ydis)^2

mds$var.explained<-round(rst*100,1)
mds$ttl.var.explained<-round(ttl*100,1)
return(mds)
}
#######################################################################
#######################################################################
#######################################################################
plot.NMDS<-function(x,env=NA,spp=NA,groups=NA,size=NA)
{
xlabel<-paste("NMDS1 (",x$var.explained[1],"%)",sep="")
ylabel<-paste("NMDS2 (",x$var.explained[2],"%)",sep="")
ord<-ordiplot(x,type='none',ylab=ylabel,xlab=xlabel)
plot.pch<-21
point.size<-1
if (!all(is.na(groups))){
 plot.pch<-groups
}
 if (!all(is.na(size))){
 point.size<-size
}
points(ord,what='sites',pch=plot.pch,cex=point.size)
#spp<-rownames(ord$species)

stress<-paste("Stress: ",round(x$stress,1))
mtext(stress,line=.2,at=min(axTicks(3)))

if (!all(is.na(env))){
 plot(env)
}
if (!all(is.na(spp))){
 spp.labels<-rownames(x$species)
 spp.labels[!spp.labels %in% spp]<-""
 text(ord,what='species',label=spp.labels)
}
}
#######################################################################
#######################################################################
#######################################################################
check.stress<-function(spp,trials,k=5:1){
 # get the actual stress for each dimension
 stress<-sapply(k,function(x){
   metaMDS(spp,k=x)$stress
 })

 rst<-data.frame(dimensions=k,stress=stress,min=NA,mean=NA,max=NA,p=NA)

 for (i in 1:length(k)){
   monte<<-sapply(1:trials,function(x){
     metaMDS(apply(spp,2,sample),k=k[i])$stress
   })
   rst[i,'min']<-min(monte)
   rst[i,'mean']<-mean(monte)
   rst[i,'max']<-max(monte)
   rst[i,'p']<-(1+sum(monte>rst[i,'stress']))/(1+trials)
 }
 scree(rst)
 return(rst)
}
#######################################################################
#######################################################################
#######################################################################
scree<-function(monte.mds.object,...){
 plot(monte.mds.object$dimensions,monte.mds.object$stress,type='b',col='blue',
xlab='Dimensions',ylab='Stress',ylim=c(0,max(c(monte.mds.object$max,monte.mds.object$stress))),...)

 for (i in 1:nrow(monte.mds.object)){
  points(rep(monte.mds.object[i,1],3),monte.mds.object[i,3:5],type='b',col='red')
 }
}
#######################################################################
#######################################################################
#######################################################################

###to run the above functions, submit all lines to R first, then run each function with your data##
mod<-NMDS(spp, k=2)  #run NMDS with 2 dimensions and Bray-Curtis distance
plot.NMDS(mod)    #plot NMDS
st<-check.stress(spp, trials=10)#it will take a while if you increase trials to 1000
scree(st)   #plot stress values with Monte Carlo results

