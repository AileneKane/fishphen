#Salmon return timing phenology using WDFW data
#Started by Ailene on March 12, 2019
# To do for orca paper:
# 1. Choose streams to include in paper. Choose streams that 
### a) have high total run sizes for wild coho, chum, chinook; 
### b) are close to puget sound
#2. See if there are multiple runs in these focal streams by looking at day of year when peak occurs
#3. Extract first, last, peak and median dates, then look at trends by fitting linear models
# Also:
# 1. Check that correlations on map are correct- some seem to have significant lines when they ar not super strong relationships...  
# 2. Plot curves of each stream/year as a separate line on same figure
# 3. Using curves, decide on different seasons to use per stream/hatchery
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
library(mgcv)
library(rworldmap)
library(scales)

# 1. Get the data
d <- read.csv("data/TrapEstimateSpawnCHCKCO.csv")
latlon<-read.csv("data/TrapEstimateSpawnCHCKCO_LatLong.csv", header=TRUE)
colnames(latlon)<-c("Facility_Short_Name","Lat","Lon")
d<-left_join(d,latlon, by="Facility_Short_Name",copy=TRUE)

# 2. Clean  and format the data

source("analyses/salmonreturns/source/clean_salmonreturns.R")

# 3. Choose what sites to focus on: creates 2 dataframes (wild, hatch) that  include list of sites for each species

source("analyses/salmonreturns/source/salmonreturns_choosesites.R")

wild.d<-left_join(wild,d)
hatch.d<-left_join(hatch,d)
types<-c("wild","hatch")
sp<-site<-type<-firstcoefsall<-lastcoefsall<-midcoefsall<-peakcoefsall<-meantotalall<-c()

for(w in 1:2){#type
  if(w==1){d2<-wild.d}
  if(w==2){d2<-hatch.d}
  

allyears<-unique(d2$BROOD_Yr)
allyears<-sort(allyears[allyears<2019])
species<-unique(d2$SPECIES_Code)
for(p in 1:length(species)){
  spdat<-d2[d2$SPECIES_Code==species[p],]
  spdat$Facility_Short_Name<-as.factor(spdat$Facility_Short_Name)
  sites<-unique(spdat$Facility_Short_Name)
for(i in 1:length(sites)){
  dat<-spdat[spdat$Facility_Short_Name==sites[i],]
  firstobsdate<-c()
  lastobsdate<-c()
  midobsdate<-c()
  peakobsdate<-c()
  alltotal<-c()
  for(y in allyears){
    datyr<-dat[dat$BROOD_Yr==y,]
    if (dim(datyr)[1]<=1){
      first<-last<-mid<-peak<-NA
      total<-NA
    }
    if (dim(datyr)[1]>0){
    count<-datyr$Adults_Cnt
    #plot(datyr$doy,count, pch=21, bg="gray", main=paste(y))
    #if(y==min(allyears)){mtext(paste(sites[i], species[p]),side=3, line=3)}
    datdoy<-datyr
    
    #for first few months of a new calendar year, want to treat is a higher doy, not a lower one, so add 365
    datdoy$doy[datdoy$BROOD_Yr==as.numeric(datdoy$year)-1 & as.numeric(datdoy$doy)<80]<-as.numeric(datdoy$doy[datdoy$BROOD_Yr==as.numeric(datdoy$year)-1 & as.numeric(datdoy$doy)<80])+365
    first<-min(datdoy$doy[which(count>0)])
    last<-max(datdoy$doy[which(count>0)])
    total<-sum(count,na.rm=TRUE)
    mid<-datdoy$doy[min(which(cumsum(count)>(total/2)))]#date at which half of fish have arrived
    peak<-min(datdoy$doy[which(count==max(count, na.rm=TRUE))])#date of peak number of fish observed, if multiple dates with same number, choose first of these
    #print(peak)
    }
    print(y);print(first);print(last);print(total); print(mid)
    #year<-c(year,y)
    firstobsdate<-c(firstobsdate,first)
    lastobsdate<-c(lastobsdate,last)
    midobsdate<-c(midobsdate,mid)
    peakobsdate<-c(peakobsdate,peak)
    alltotal<-c(alltotal,total)
    
    firstobsdate[which(firstobsdate=="Inf")]<-NA
    peakobsdate[which(peakobsdate=="Inf")]<-NA
    lastobsdate[which(lastobsdate=="-Inf")]<-NA
  }
  #if(length(which(is.na(firstobsdate)))<5){next}
  year<-as.numeric(allyears)
  #figname<-paste("analyses/figures/wdfw_returns/",types[w],species[p],sites[i],".pdf", sep="_")
  #pdf(figname,height=10, width=25)
  quartz(height=8, width=30)
  par(mfrow=c(1,5), oma=c(1,1,1,1))
  hist(dat$doy)
  plot(year,firstobsdate,pch=21, bg="gray", main=paste(species[p],sites[i],types[w]))
  
  if(length(which(!is.na(firstobsdate)))>3){
    firstmod<-lm(firstobsdate~year)
      if(summary(firstmod)$coef[2,4]<0.10){
      abline(firstmod)
      text(max(year)-1,min(firstobsdate, na.rm=TRUE),labels=paste("coef=",round(coef(firstmod)[2], digits=2), sep=""), cex=1.2)
      }
    firstcoefs<-coef(firstmod)
    firstcoefs.ci<-confint(firstmod,level = 0.75)
  }
  if(length(which(!is.na(firstobsdate)))<=3){
    firstcoefs<-c(NA,NA)
    firstcoefs.ci<-rbind(c(NA,NA),c(NA,NA))
  }
  
  plot(year,lastobsdate,pch=21, bg="gray")
    
  if(length(which(!is.na(lastobsdate)))>3){
    lastmod<-lm(lastobsdate~year)
      if(summary(lastmod)$coef[2,4]<0.10){
      abline(lastmod)
      text(max(year)-1,min(lastobsdate, na.rm=TRUE),labels=paste("coef=",round(coef(lastmod)[2], digits=2), sep=""), cex=1.2)
      }
    lastcoefs<-coef(lastmod)
    lastcoefs.ci<-confint(lastmod,level = 0.75)
  }
    if(length(which(!is.na(lastobsdate)))<=3){
      lastcoefs<-c(NA,NA)
      lastcoefs.ci<-rbind(c(NA,NA),c(NA,NA))
    } 
  
  #mid observation date
  if(length(unique(midobsdate))!=1){
  plot(year,midobsdate,pch=21, bg="gray")}
  if(length(unique(midobsdate))==1){
    plot(year,rep(1,length(year)),pch=21, bg="gray")}
  
 if(length(which(!is.na(midobsdate)))>3){
    midmod<-lm(midobsdate~year)
      if(!is.na(summary(midmod)$coef[2,4]) & summary(midmod)$coef[2,4]<0.10){
      abline(midmod)
      text(max(year)-1,min(midobsdate, na.rm=TRUE),labels=paste("coef=",round(coef(midmod)[2], digits=2), sep=""), cex=1.2)
    }
    midcoefs<-coef(midmod)
    midcoefs.ci<-confint(midmod,level = 0.75)
  }  
  if(length(which(!is.na(midobsdate)))<=3){
    midcoefs<-c(NA,NA)
    midcoefs.ci<-rbind(c(NA,NA),c(NA,NA))
  } 
  
  #peak observation date
  plot(year,peakobsdate,pch=21, bg="gray")
  if(length(which(!is.na(peakobsdate)))>3){
    peakmod<-lm(peakobsdate~year)
    if(summary(peakmod)$coef[2,4]<0.10){
      abline(peakmod)
      text(max(year)-1,min(peakobsdate, na.rm=TRUE),labels=paste("coef=",round(coef(peakmod)[2], digits=2), sep=""), cex=1.2)
    }
    peakcoefs<-coef(peakmod)
    peakcoefs.ci<-confint(peakmod,level = 0.75)
  } 
  if(length(which(!is.na(peakobsdate)))<=3){
    peakcoefs<-c(NA,NA)
    peakcoefs.ci<-rbind(c(NA,NA),c(NA,NA))
  } 
  #dev.off()
  if(length(which(!is.na(midobsdate)))>0){
    meantotal<-mean(alltotal, na.rm=TRUE)
  }  
  if(length(which(!is.na(midobsdate)))<=0){
    meantotal<-NA
  } 
  
  #save coefs to make a table
  firstcoefsall<-rbind(firstcoefsall,c(firstcoefs[1],firstcoefs.ci[1,],firstcoefs[2],firstcoefs.ci[2,]))

  lastcoefsall<-rbind(lastcoefsall,c(lastcoefs[1],lastcoefs.ci[1,],lastcoefs[2],lastcoefs.ci[2,]))
  
  midcoefsall<-rbind(midcoefsall,c(midcoefs[1],midcoefs.ci[1,],midcoefs[2],midcoefs.ci[2,]))
  
  peakcoefsall<-rbind(peakcoefsall,c(peakcoefs[1],peakcoefs.ci[1,],peakcoefs[2],peakcoefs.ci[2,]))
  
  meantotalall<-c(meantotalall,meantotal)
  
  sp<-c(sp,species[p])
  site<-c(site, as.character(sites[i]))
  type<-c(type,types[w])
  }
}

#make a big table
allmodsums<-as.data.frame(cbind(type,sp,site,meantotalall,round(firstcoefsall, digits=3),round(lastcoefsall, digits=3),
                                round(midcoefsall, digits=3),round(peakcoefsall, digits=3)))
colnames(allmodsums)[4:28]<-c("mn.total.runsize","first.int", "first.intlci","first.intuci","first.yr", "first.yrlci","first.yruci",
                              "last.int", "last.intlci","last.intuci","last.yr", "last.yrlci","last.yruci",
                              "mid.int", "mid.intlci","mid.intuci","mid.yr", "mid.yrlci","mid.yruci",
                              "pk.int", "pk.intlci","pk.intuci","pk.yr", "pk.yrlci","pk.yruci")

#Add priority stocks column for chinook
if(w==1){write.csv(allmodsums, "analyses/output/salmonreturntrends_wild.csv", row.names = FALSE)}
if(w==2){write.csv(allmodsums[allmodsums$type=="hatch",], "analyses/output/salmonreturntrends_hatch.csv", row.names = FALSE)}

}


#The below has not been updated recently
# Make annual abundance curves, to look at variation, of individual streams and of Puget Sound vs Upper Salish Sea as a whole
modsums$mn.total.runsize
head(modsums)

#Plot the data and look at it and pull out first and last observation date
d$doy<-as.integer(d$doy)
allyears<-sort(allyears[allyears<2019])
#different colored points/lines for each site
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
cols=sample(color,length(unique(datyr$Facility_Short_Name)))
#d$cols<-cols
sp<-site<-c()
species<-unique(d$SPECIES_Code)
for(p in 1:length(species)){
  spdat<-d[d$SPECIES_Code==species[p],]
  spdat<-spdat[order(spdat$year,spdat$doy),]
  years<-sort(unique(spdat$BROOD_Yr))
  quartz(height=10,width=25)
  par(mfrow=c(3,8),oma=c(1,1,1,1))
   for(y in years){
   datyr<-spdat[spdat$BROOD_Yr==y,]
   datyr$Facility_Short_Name<-as.factor(datyr$Facility_Short_Name)
   sites<-unique(datyr$Facility_Short_Name)
   
    plot(datyr$doy,datyr$Adults_Cnt, pch=21, xlab="DOY",ylab="# Adults",bg=cols[as.numeric(datyr$Facility_Short_Name)], main=paste(y))
    if(y==min(years)){mtext(paste(species[p]),side=2, line=4)}
       #fit a gam for each site and plot curves
    for(i in 1:length(sites)){
        sitedat<-datyr[datyr$Facility_Short_Name==sites[i],]
        sitedat<-sitedat[-which(is.na(sitedat$Adults_Cnt)),]
        if(length(sitedat$Adults_Cnt)<12){next}
        g = gam(sitedat$Adults_Cnt ~ s(sitedat$doy), family="poisson")
        lines(sitedat$doy,g$fitted.values,lwd=3, col=cols[i])
    }
     }
     
   }
