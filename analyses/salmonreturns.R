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

unique(d$year)#goes back to 1998
unique(d$ADULT_EVENT_TYPE_Name)#Trap estimates and PArent Spawn EVents..not sure what the difference is
table(d$year,d$ADULT_EVENT_TYPE_Name)
unique(d$MarkCode)#should find out what these mean...
tapply(d$Adults_Cnt, list(d$year,d$Facility_Short_Name),sum, na.rm=TRUE)
#The following hatcheries seem to have large amounts of continuous data and be close to puget sound:
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

for(i in 1:length(sites)){
  dat<-dps[dps$Facility_Short_Name==sites[i],]
  quartz(height=10, width=20)
  par(mfrow=c(4,6), oma=c(1,1,3,1))
  year<-c()
  firstobsdate<-c()
  lastobsdate<-c()
  midobsdate<-c()
  for(y in years){
    datyr<-dat[dat$year==y,]
   plot(datyr$doy,datyr$Females_Cnt, pch=21, bg="gray", main=paste(y))
    if(y==min(years)){mtext(paste(sites[i]),side=3, line=3)}
    datyr<-datyr[which(!is.na(datyr$Females_Cnt)),]
    datdoy<-aggregate(datyr$Females_Cnt,by=list(datyr$doy),sum, na.rm=TRUE)
    colnames(datdoy)<-c("doy","Females_Cnt")
    points(datdoy$doy,datdoy$Females_Cnt, pch=21, bg="red", cex=1.2,main=paste(y))
    first<-min(datdoy$doy[which(datdoy$Females_Cnt>0)])
    last<-max(datdoy$doy[which(datdoy$Females_Cnt>0)])
    total<-sum(datdoy$Females_Cnt,na.rm=TRUE)
    mid<-datdoy$doy[min(which(cumsum(datdoy$Females_Cnt)>(total/2)))]#date at which half of fish have arrived
    year<-c(year,y)
    firstobsdate<-c(firstobsdate,first)
    lastobsdate<-c(lastobsdate,last)
    midobsdate<-c(midobsdate,mid)
  }
  year<-as.numeric(year)
  quartz(height=10, width=20)
  par(mfrow=c(1,3), oma=c(1,1,1,1))
  plot(year,firstobsdate,pch=21, bg="gray")
  firstmod<-lm(firstobsdate~year)
  if(summary(firstmod)$coef[2,4]<0.10){
    abline(firstmod)
     text(max(year)-1,min(firstobsdate),labels=paste("coef=",round(coef(firstmod)[2], digits=2), sep=""), cex=1.2)
    }
  plot(year,lastobsdate,pch=21, bg="gray", main=paste(sites[i]))
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
}
#some are getting later and som are getting earlier
#Voights:  mid getting earlier
#Tumwater: All getting earlier (first, last, mid)
#minter: first getting earlier, mid getting later
#soos: first getting earlier, mid getting earlier
#hoodsport: first getting later
i=4
