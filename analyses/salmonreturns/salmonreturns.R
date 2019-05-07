#Salmon return timing phenology using WDFW data
#Started by Ailene on March 12, 2019

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
# 1. Get the data
d <- read.csv("data/TrapEstimateSpawnCHCKCO.csv")
head(d)

#Create columns for month, year, doy
d$doy<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%j")
d$year<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%Y")
d$month<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%m")

unique(d$year)#goes back to 1995
unique(d$ADULT_EVENT_TYPE_Name)#Trap estimates and Parent Spawn EVents..not sure what the difference is.. ask DFW which I should use...
#just use trap estimates
d<-d[d$ADULT_EVENT_TYPE_Name=="Trap Estimate",]
unique(d$MarkCode)#should find out what these mean...
#which column should i use- adults_Cnt, Females_Cnt, Males_Cnt
#length(which(is.na(d$Adults_Cnt)))
#length(which(is.na(d$Males_Cnt)))
#length(which(is.na(d$Females_Cnt)))
#definitely ADults_Cnt- ahs way more data!
#When Adults_Cnt, does not have data, does Females_Cnt have data?
#length(which(is.na(d$Females_Cnt[which(is.na(d$Adults_Cnt))])))
#unique(d$Females_Cnt[which(is.na(d$Adults_Cnt))])
#nope- all NAs!
# 2. A bit of cleaning: Brood year seems to be off in a few cases
d$year[d$BROOD_Yr==1997 & d$year==2000]<-1997#i think the brood year is correct because all other nearby rows have year=1997
d$BROOD_Yr[d$BROOD_Yr==1993]<-d$year[d$BROOD_Yr==1993]#
d$BROOD_Yr[d$BROOD_Yr==2003 & d$year==2002]<-2002
d$BROOD_Yr[d$BROOD_Yr==1997 & d$year==1998 & d$doy>250]<-1998

#in most cases, the Brood year column seems to align with the spawning year in the way i want it to, but occasionally there are mistakes in it
#especially in 1997-1998. fix this

#The following hatcheries seem to have large amounts of continuous data and seem to be close to puget sound:
#dps<-d[d$Facility_Short_Name=="HOODSPORT HATCHERY"|d$Facility_Short_Name=="ISSAQUAH HATCHERY"|d$Facility_Short_Name=="MINTER CR HATCHERY"|
#         d$Facility_Short_Name=="SOOS CREEK HATCHERY"|d$Facility_Short_Name=="TUMWATER FALLS HATCHERY"|d$Facility_Short_Name=="VOIGHTS CR HATCHERY"|
#         d$Facility_Short_Name=="WALLACE R HATCHERY",]
#dim(dps)
dps<-d
#other hatcheries that seem to have continuous data: BINGHAM CR HATCHERY#COWLITZ SALMON HATCHERY#DUNGENESS HATCHERY#EASTBANK HATCHERY#ELOCHOMAN HATCHERY#ELWHA HATCHERY#FALLERT CR HATCHERY #FORKS CREEK HATCHERY#GARRISON HATCHERY 
#GRAYS RIVER HATCHERY #HUMPTULIPS HATCHERY#HUPP SPRINGS REARING#KALAMA FALLS HATCHERY #KENDALL CR HATCHERY #KLICKITAT HATCHERY#LEWIS RIVER HATCHERY#K ABERDEEN HATCHERY #LYONS FERRY HATCHERY #MARBLEMOUNT HATCHERY#MCKERNAN HATCHERY#METHOW HATCHERY 
#NASELLE HATCHERY #NEMAH HATCHERY#NORTH TOUTLE HATCHERY#PRIEST RAPIDS HATCHERY#RINGOLD SPRINGS HATCHERY #SAMISH HATCHERY#SOLDUC HATCHERY #WASHOUGAL HATCHERY#WELLS HATCHERY
#Plot the data and look at it and pull out first and last observation date
dps$doy<-as.integer(dps$doy)
allyears<-unique(dps$BROOD_Yr)
allyears<-sort(allyears[allyears<2019])
#7 hatcheries
sp<-site<- firstcoefsall<-lastcoefsall<-midcoefsall<-peakcoefsall<-c()
species<-unique(dps$SPECIES_Code)
for(s in 1:length(species)){
  spdat<-dps[dps$SPECIES_Code==species[s],]
  sites<-unique(spdat$Facility_Short_Name)
for(i in 1:length(sites)){
  dat<-spdat[spdat$Facility_Short_Name==sites[i],]
  firstobsdate<-c()
  lastobsdate<-c()
  midobsdate<-c()
  peakobsdate<-c()
  for(y in allyears){
    datyr<-dat[dat$BROOD_Yr==y,]
    if (dim(datyr)[1]<=1){
      first<-last<-mid<-peak<-NA
      total<-NA
    }
    if (dim(datyr)[1]>0){
    count<-datyr$Adults_Cnt
    #plot(datyr$doy,count, pch=21, bg="gray", main=paste(y))
    #if(y==min(allyears)){mtext(paste(sites[i], species[s]),side=3, line=3)}
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
    print(y);print(first);print(last);print(total); print(total); print(mid)
    #year<-c(year,y)
    
    firstobsdate<-c(firstobsdate,first)
    lastobsdate<-c(lastobsdate,last)
    midobsdate<-c(midobsdate,mid)
    peakobsdate<-c(peakobsdate,peak)
    firstobsdate[which(firstobsdate=="Inf")]<-NA
    peakobsdate[which(peakobsdate=="Inf")]<-NA
    lastobsdate[which(lastobsdate=="-Inf")]<-NA
  }
  #if(length(which(is.na(firstobsdate)))<5){next}
  year<-as.numeric(allyears)
  figname<-paste("analyses/figures/wdfw_returns/",species[s],sites[i],".pdf", sep="")
  pdf(figname,height=10, width=25)
  #quartz(height=10, width=25)
  par(mfrow=c(1,4), oma=c(1,1,1,1))
  plot(year,firstobsdate,pch=21, bg="gray", main=paste(species[s],sites[i]))
  
  if(length(which(!is.na(firstobsdate)))>3){
    firstmod<-lm(firstobsdate~year)
      if(summary(firstmod)$coef[2,4]<0.10){
      abline(firstmod)
      text(max(year)-1,min(firstobsdate, na.rm=TRUE),labels=paste("coef=",round(coef(firstmod)[2], digits=2), sep=""), cex=1.2)
      }
    firstcoefs<-coef(firstmod)
    firstcoefs.ci<-confint(firstmod)
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
    lastcoefs.ci<-confint(lastmod)
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
    midcoefs.ci<-confint(midmod)
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
    peakcoefs.ci<-confint(peakmod)
  } 
  if(length(which(!is.na(peakobsdate)))<=3){
    peakcoefs<-c(NA,NA)
    peakcoefs.ci<-rbind(c(NA,NA),c(NA,NA))
  } 
  dev.off()
  #save coefs to make a table
  firstcoefsall<-rbind(firstcoefsall,c(firstcoefs[1],firstcoefs.ci[1,],firstcoefs[2],firstcoefs.ci[2,]))

  lastcoefsall<-rbind(lastcoefsall,c(lastcoefs[1],lastcoefs.ci[1,],lastcoefs[2],lastcoefs.ci[2,]))
  
  midcoefsall<-rbind(midcoefsall,c(midcoefs[1],midcoefs.ci[1,],midcoefs[2],midcoefs.ci[2,]))
  
  peakcoefsall<-rbind(peakcoefsall,c(peakcoefs[1],peakcoefs.ci[1,],peakcoefs[2],peakcoefs.ci[2,]))
  
  sp<-c(sp,species[s])
  site<-c(site, sites[i])
  }
}

#make a big table
allmodsums<-as.data.frame(cbind(sp,site,round(firstcoefsall, digits=3),round(lastcoefsall, digits=3),
                                round(midcoefsall, digits=3),round(peakcoefsall, digits=3)))
colnames(allmodsums)[3:26]<-c("first.int", "first.intlci","first.intuci","first.yr", "first.yrlci","first.yruci",
                              "last.int", "last.intlci","last.intuci","last.yr", "last.yrlci","last.yruci",
                              "mid.int", "mid.intlci","mid.intuci","mid.yr", "mid.yrlci","mid.yruci",
                              "pk.int", "pk.intlci","pk.intuci","pk.yr", "pk.yrlci","pk.yruci")

#Add Lat/Long to the table
latlon<-read.csv("data/TrapEstimateSpawnCHCKCO_LatLong.csv", header=TRUE)
colnames(latlon)<-c("site","lat","lon")
modsums<-left_join(allmodsums,latlon, by="site",copy=TRUE)
write.csv(modsums, "analyses/output/salmonreturntrends.csv", row.names = FALSE)
unique(allmodsums$site)
#write csv of list of sites to ask for lat/longs
#write.csv(allmodsums$site, "analyses/output/salmonreturnsites.csv", row.names = FALSE)

#some are getting later and some are getting earlier
#Voights:  mid getting earlier
#Tumwater: All getting earlier (first, last, mid)
#minter: first getting earlier, mid getting later
#soos: first getting earlier, mid getting earlier
#hoodsport: first getting later

