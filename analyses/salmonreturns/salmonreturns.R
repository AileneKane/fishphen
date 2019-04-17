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

# 1. Get the data
d <- read.csv("data/TrapEstimate&SpawnCHCKCO.csv")
head(d)
#Create columns for month, year, doy
d$doy<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%j")
d$year<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%Y")
d$month<-strftime(strptime(d$Event_Date,format= "%m/%d/%y"),format= "%m")

unique(d$year)#goes back to 1995
unique(d$ADULT_EVENT_TYPE_Name)#Trap estimates and PArent Spawn EVents..not sure what the difference is.. ask DFW which I should use...
table(d$year,d$ADULT_EVENT_TYPE_Name)
unique(d$MarkCode)#should find out what these mean...
tapply(d$Adults_Cnt, list(d$year,d$Facility_Short_Name),sum, na.rm=TRUE)
#The following hatcheries seem to have large amounts of continuous data and seem to be close to puget sound:
dps<-d[d$Facility_Short_Name=="HOODSPORT HATCHERY"|d$Facility_Short_Name=="ISSAQUAH HATCHERY"|d$Facility_Short_Name=="MINTER CR HATCHERY"|
         d$Facility_Short_Name=="SOOS CREEK HATCHERY"|d$Facility_Short_Name=="TUMWATER FALLS HATCHERY"|d$Facility_Short_Name=="VOIGHTS CR HATCHERY"|
         d$Facility_Short_Name=="WALLACE R HATCHERY",]
dim(dps)
#other hatcheries that seem to have continuous data: BINGHAM CR HATCHERY#COWLITZ SALMON HATCHERY#DUNGENESS HATCHERY#EASTBANK HATCHERY#ELOCHOMAN HATCHERY#ELWHA HATCHERY#FALLERT CR HATCHERY #FORKS CREEK HATCHERY#GARRISON HATCHERY 
#GRAYS RIVER HATCHERY #HUMPTULIPS HATCHERY#HUPP SPRINGS REARING#KALAMA FALLS HATCHERY #KENDALL CR HATCHERY #KLICKITAT HATCHERY#LEWIS RIVER HATCHERY#K ABERDEEN HATCHERY #LYONS FERRY HATCHERY #MARBLEMOUNT HATCHERY#MCKERNAN HATCHERY#METHOW HATCHERY 
#NASELLE HATCHERY #NEMAH HATCHERY#NORTH TOUTLE HATCHERY#PRIEST RAPIDS HATCHERY#RINGOLD SPRINGS HATCHERY #SAMISH HATCHERY#SOLDUC HATCHERY #WASHOUGAL HATCHERY#WELLS HATCHERY
#Plot the data and look at it and pull out first and last observation date
dps$doy<-as.integer(dps$doy)
allyears<-unique(dps$year)
sites<-unique(dps$Facility_Short_Name)#7 hatcheries
sp<-site<-firstcoefs<-firstcoefs.ci<-lastcoefs<-lastcoefs.ci<-midcoefs<-midcoefs.ci<-peakcoefs<-peakcoefs.ci<-c()
species<-unique(dps$SPECIES_Code)
for(s in 1:length(species)){
  spdat<-dps[dps$SPECIES_Code==species[s],]
for(i in 1:length(sites)){
  dat<-spdat[spdat$Facility_Short_Name==sites[i],]
  quartz(height=10, width=20)
  par(mfrow=c(4,6), oma=c(1,1,3,1))
  year<-c()
  firstobsdate<-c()
  lastobsdate<-c()
  midobsdate<-c()
  peakobsdate<-c()
  for(y in allyears){
    datyr<-dat[dat$year==y,]
  if(dim(datyr)[1]==0){next}
   count<-datyr$Females_Cnt
   if(length(unique(datyr$Females_Cnt))==1) {if(is.na(unique(datyr$Females_Cnt))){count<-datyr$Adults_Cnt}}
    #plot(datyr$doy,count, pch=21, bg="gray", main=paste(y))
    #if(y==min(allyears)){mtext(paste(sites[i], species[s]),side=3, line=3)}
    datdoy<-datyr
    #datyr<-datyr[which(!is.na(datyr$Females_Cnt)),]
    #datdoy<-aggregate(datyr$Females_Cnt,by=list(datyr$doy),sum, na.rm=TRUE)
    #colnames(datdoy)<-c("doy","Females_Cnt")
    #points(datdoy$doy,count, pch=21, bg="red", cex=1.2,main=paste(y))
    first<-min(datdoy$doy[which(count>0)])
    last<-max(datdoy$doy[which(count>0)])
    total<-sum(count,na.rm=TRUE)
    mid<-datdoy$doy[min(which(cumsum(count)>(total/2)))]#date at which half of fish have arrived
    peak<-min(datdoy$doy[which(count==max(count, na.rm=TRUE))])#date of peak number of fish observed, if multiple dates with same number, choose first of these
    #print(peak)
    year<-c(year,y)
    firstobsdate<-c(firstobsdate,first)
    lastobsdate<-c(lastobsdate,last)
    midobsdate<-c(midobsdate,mid)
    peakobsdate<-c(peakobsdate,peak)
  }
  if(length(year)<5){next}
  year<-as.numeric(year)
  #quartz(height=10, width=25)
  figname<-paste("analyses/figures/wdfw_returns/",sp[s],site[i],".pdf", sep="")
  pdf(figname,height=10, width=25)
  par(mfrow=c(1,4), oma=c(1,1,1,1))
  plot(year,firstobsdate,pch=21, bg="gray")
  firstmod<-lm(firstobsdate~year)
  if(summary(firstmod)$coef[2,4]<0.10){
    abline(firstmod)
     text(max(year)-1,min(firstobsdate),labels=paste("coef=",round(coef(firstmod)[2], digits=2), sep=""), cex=1.2)
    }
  plot(year,lastobsdate,pch=21, bg="gray", main=paste(species[s],sites[i]))
  lastmod<-lm(lastobsdate~year)
  if(summary(lastmod)$coef[2,4]<0.10){
    abline(lastmod)
    text(max(year)-1,min(lastobsdate),labels=paste("coef=",round(coef(lastmod)[2], digits=2), sep=""), cex=1.2)
    }
  plot(year,midobsdate,pch=21, bg="gray")
  midmod<-lm(midobsdate~year)
  if(summary(midmod)$coef[2,4]<0.10){
    abline(midmod)
    text(max(year)-1,min(midobsdate),labels=paste("coef=",round(coef(midmod)[2], digits=2), sep=""), cex=1.2)
  }
  plot(year,peakobsdate,pch=21, bg="gray")
  peakmod<-lm(peakobsdate~year)
  if(summary(peakmod)$coef[2,4]<0.10){
    abline(peakmod)
    text(max(year)-1,min(peakobsdate),labels=paste("coef=",round(coef(peakmod)[2], digits=2), sep=""), cex=1.2)
  }
  dev.off()
  #save coefs to make a table
  firstcoefs<-c(firstcoefs,coef(firstmod)[2])
  firstcoefs.ci<-rbind(firstcoefs.ci,confint(firstmod)[2,])
  lastcoefs<-c(lastcoefs,coef(lastmod)[2])
  lastcoefs.ci<-rbind(lastcoefs.ci,confint(lastmod)[2,])
  midcoefs<-c(midcoefs,coef(midmod)[2])
  midcoefs.ci<-rbind(midcoefs.ci,confint(midmod)[2,])
  peakcoefs<-c(peakcoefs,coef(peakmod)[2])
  peakcoefs.ci<-rbind(peakcoefs.ci,confint(peakmod)[2,])
  sp<-c(sp,species[s])
  site<-c(site, sites[i])
  }
}

#make a big table
allmodsums<-cbind(sp,site,firstcoefs,firstcoefs.ci,lastcoefs,lastcoefs.ci,midcoefs,midecoefs.ci,peakcoefs,peakcoefs.ci)

#some are getting later and some are getting earlier
#Voights:  mid getting earlier
#Tumwater: All getting earlier (first, last, mid)
#minter: first getting earlier, mid getting later
#soos: first getting earlier, mid getting earlier
#hoodsport: first getting later

