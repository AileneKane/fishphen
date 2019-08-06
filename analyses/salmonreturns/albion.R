#Code to analyse the fraser river test fishery data to get estimates of first, last, median dates of spring chinook runs
#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
setwd("~/Documents/GitHub/fishphen")
#or from laptop:
#setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
# 1. Read in the datafiles
source("analyses/salmonreturns/source/read_albiondat.R")
dim(d)#6148
unique(d$calDay)
allyears<-unique(d$year)
dat<-d
firstobsdate<-c()
lastobsdate<-c()
midobsdate<-c()
peakobsdate<-c()
alltotal<-c()
for(y in allyears){
  datyr<-dat[dat$year==y,]
  if (dim(datyr)[1]<=1){
    first<-last<-mid<-peak<-NA
    total<-NA
  }
  if (dim(datyr)[1]>0){
    cpue<-datyr$cpue
    #plot(datyr$doy,count, pch=21, bg="gray", main=paste(y))
    #if(y==min(allyears)){mtext(paste(sites[i], species[p]),side=3, line=3)}
    datdoy<-datyr
    first<-min(datdoy$calDay[which(cpue>0)])
    last<-max(datdoy$calDay[which(cpue>0)])
    total<-sum(cpue,na.rm=TRUE)
    mid<-datdoy$calDay[min(which(cumsum(cpue)>(total/2)))]#date at which half of fish have arrived
    peak<-min(datdoy$calDay[which(cpue==max(cpue, na.rm=TRUE))])#date of peak number of fish observed, if multiple dates with same number, choose first of these
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
hist(dat$calDay)
plot(year,firstobsdate,pch=21, bg="gray")

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
if(length(which(!is.na(cpue)))>0){
  meantotal<-mean(cpue, na.rm=TRUE)
}  
if(length(which(!is.na(cpue)))<=0){
  meantotal<-NA
} 

#save coefs to make a table
firstcoefsall<-c(firstcoefs[1],firstcoefs.ci[1,],firstcoefs[2],firstcoefs.ci[2,])

lastcoefsall<-c(lastcoefs[1],lastcoefs.ci[1,],lastcoefs[2],lastcoefs.ci[2,])

midcoefsall<-c(midcoefs[1],midcoefs.ci[1,],midcoefs[2],midcoefs.ci[2,])

peakcoefsall<-c(peakcoefs[1],peakcoefs.ci[1,],peakcoefs[2],peakcoefs.ci[2,])
#add total catch (not effort corrected)
total.year<-aggregate(d$catch,by=list(d$year), sum)
mn.total<-mean(total.year$x)


allmodsums<-c("ck","albion",meantotal,mn.total,round(firstcoefsall, digits=3),round(lastcoefsall, digits=3),
                                round(midcoefsall, digits=3),round(peakcoefsall, digits=3))
names(allmodsums)<-c("sp","site","mn.cpue","mn.total","meanfirst.int","first.intlci","first.intuci","first.yr", "first.yrlci","first.yruci",
                              "last.int", "last.intlci","last.intuci","last.yr", "last.yrlci","last.yruci",
                              "mid.int", "mid.intlci","mid.intuci","mid.yr", "mid.yrlci","mid.yruci",
                              "pk.int", "pk.intlci","pk.intuci","pk.yr", "pk.yrlci","pk.yruci")


write.csv(allmodsums, "analyses/output/albionreturntrends.csv", row.names = TRUE)
