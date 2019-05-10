#Make a map of shifts in salmon return timing
#Started by Ailene on March 12, 2019

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(rworldmap)
library(scales)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

shifts<-read.csv( "analyses/output/salmonreturntrends.csv", header=TRUE)
head(shifts)

#make a map that shows the shifts for each phenophase
#For now just plot lat longs of points as xs and ys
#Chinook first:
figname<-paste("analyses/figures/wdfw_returns/salmon.shifts.map.pdf")
pdf(figname,height=10, width=25)

#quartz(height=15, width=25)
par(mfrow=c(3,4), oma=c(1,1,1,1))
phase<-c("first","last","mid","peak")
phasecols<-c(8,14,20,26)
sp<-unique(shifts$sp)
for(s in 1:length(sp)){
  spdat<-shifts[shifts$sp==sp[s],]
  
  for(p in 1:length(phase)){
  plot(spdat$lon,spdat$lat,type="p",pch=21, xlab="Longitude",ylab="Latitude", cex=2, main=paste(phase[p],sp[s]))
  #select out the sites with credible intervals that do not overlap 0 and split them into earlier and later
  earlier<-which(sign(spdat[,phasecols[p]])<0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))
  later<-which(sign(spdat[,phasecols[p]])>0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))

  points(spdat$lon[earlier],spdat$lat[earlier],type="p",pch=21,bg=alpha("darkred",.4), cex=2)
  points(spdat$lon[later],spdat$lat[later],type="p",pch=21,bg=alpha("lightblue",.4), cex=2)
  }
}
legend(c(-120,49),legend=c("shifting earlier","shifting later", "no change"), 
       pt.bg=alpha(c("darkred","lightblue","white"), .2), pch=21, pt.cex=1.5)

dev.off()

#Same thing, but with points sized by mean total run size
figname<-paste("analyses/figures/wdfw_returns/salmon.shifts.mapsize.pdf")
pdf(figname,height=10, width=25)

#quartz(height=15, width=25)
par(mfrow=c(3,4), oma=c(1,1,1,1))
phase<-c("first","last","mid","peak")
phasecols<-c(8,14,20,26)
sp<-unique(shifts$sp)
for(s in 1:length(sp)){
  spdat<-shifts[shifts$sp==sp[s],]
  
  for(p in 1:length(phase)){
    plot(spdat$lon,spdat$lat,type="p",pch=21, xlab="Longitude",ylab="Latitude", cex=log(spdat$mn.total, base=10), main=paste(phase[p],sp[s]))
    #select out the sites with credible intervals that do not overlap 0 and split them into earlier and later
    earlier<-which(sign(spdat[,phasecols[p]])<0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))
    later<-which(sign(spdat[,phasecols[p]])>0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))
    
    points(spdat$lon[earlier],spdat$lat[earlier],type="p",pch=21,bg=alpha("darkred",.4), cex=log(spdat$mn.total, base=10))
    points(spdat$lon[later],spdat$lat[later],type="p",pch=21,bg=alpha("lightblue",.4), cex=log(spdat$mn.total, base=10))
  }
}
legend(-120,49,legend=c(" shifting earlier"," shifting later", " no change", " Run size = 10"," Run size = 1,000"),
       pch=21, bty="n",pt.bg=alpha(c("darkred","lightblue","white","white","white"), .4), pt.cex=c(1.5,1.5,1.5,log(10, base=10), log(10000, base=10)))

dev.off()

newmap <- getMap(resolution = "low")

figname<-paste("analyses/figures/wdfw_returns/salmon.shifts.mapwasize.pdf")
pdf(figname,height=10, width=25)

#quartz(height=15, width=25)
par(mfrow=c(3,4), oma=c(1,1,1,1))
for(s in 1:length(sp)){
  spdat<-shifts[shifts$sp==sp[s],]
  
  for(p in 1:length(phase)){
    
    plot(newmap, xlim = c(-125, -117), ylim = c(45, 49), asp = 1,main=paste(phase[p],sp[s]))
    points(spdat$lon,spdat$lat,type="p",pch=21, cex=2)
    #select out the sites with credible intervals that do not overlap 0 and split them into earlier and later
    earlier<-which(sign(spdat[,phasecols[p]])<0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))
    later<-which(sign(spdat[,phasecols[p]])>0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))
    points(spdat$lon[earlier],spdat$lat[earlier],type="p",pch=21,bg="darkred", cex=2)
    points(spdat$lon[later],spdat$lat[later],type="p",pch=21,bg="lightblue", cex=2)
  }
}
dev.off()

#now with points sized by total run size

figname<-paste("analyses/figures/wdfw_returns/salmon.shifts.mapwasize.pdf")
pdf(figname,height=10, width=25)

#quartz(height=15, width=25)
par(mfrow=c(3,4), oma=c(1,1,1,1))
for(s in 1:length(sp)){
  spdat<-shifts[shifts$sp==sp[s],]
  
  for(p in 1:length(phase)){
    
    plot(newmap, xlim = c(-125, -117), ylim = c(45, 49), asp = 1,main=paste(phase[p],sp[s]))
    points(spdat$lon,spdat$lat,type="p",pch=21, cex=log(spdat$mn.total, base=10), main=paste(phase[p],sp[s]))
    #select out the sites with credible intervals that do not overlap 0 and split them into earlier and later
    earlier<-which(sign(spdat[,phasecols[p]])<0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))
    later<-which(sign(spdat[,phasecols[p]])>0 & sign(spdat[,phasecols[p]])==sign(spdat[,phasecols[p]+1]))
    
    points(spdat$lon[earlier],spdat$lat[earlier],type="p",pch=21,bg=alpha("darkred",.4), cex=log(spdat$mn.total, base=10))
    points(spdat$lon[later],spdat$lat[later],type="p",pch=21,bg=alpha("lightblue",.4), cex=log(spdat$mn.total, base=10))
  }
}
legend(-120,49,legend=c(" shifting earlier"," shifting later", " no change", " Run size = 10"," Run size = 1,000"),
       pch=21, bty="n",pt.bg=alpha(c("darkred","lightblue","white","white","white"), .4), pt.cex=c(1.5,1.5,1.5,log(10, base=10), log(10000, base=10)))


dev.off()

newmap <- getMap(resolution = "low")

#size points by total run size
