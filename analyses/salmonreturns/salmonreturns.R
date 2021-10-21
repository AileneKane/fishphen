#Salmon return timing phenology using WDFW data
#Started by Ailene on March 12, 2019

#housekeeping

rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Set working directory: 
#setwd("~/Documents/GitHub/fishphen")
#or from laptop:
setwd("/Users/aileneettinger/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
library(mgcv)
library(rworldmap)
library(scales)
library(lme4)
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
wild.d$Name.Sp<-paste(wild.d$Facility_Short_Name,wild.d$SPECIES_Code, sep=".")
hatch.d$Name.Sp<-paste(hatch.d$Facility_Short_Name,hatch.d$SPECIES_Code, sep=".")
#Remove sites outside of puget sound proper
wild.d<-wild.d[!wild.d$Facility_Short_Name=="ELWHA HATCHERY",]
hatch.d<-hatch.d[!hatch.d$Facility_Short_Name=="ELWHA HATCHERY",]
hatch.d<-hatch.d[!hatch.d$Facility_Short_Name=="GLENWOOD SPRINGS",]
hatch.d<-hatch.d[!hatch.d$Facility_Short_Name=="WHATCOM CR HATCHERY",]

#create a table of the sites, lat/long, 
wildtab<- wild.d %>% # start with the data frame
  distinct(Name.Sp, .keep_all = TRUE) %>% # establishing grouping variables
  #filter(continent == 'europe' & year >= 1950) %>%#select out europe#this removes all rows with  field sample date=2013-10-22, even though some are in europe
  dplyr::select(Facility_Short_Name,SPECIES_Code,ORIGIN_TYPE_Code,Lat,Lon)
hatchtab<- hatch.d %>% # start with the data frame
  distinct(Name.Sp, .keep_all = TRUE) %>% # establishing grouping variables
  #filter(continent == 'europe' & year >= 1950) %>%#select out europe#this removes all rows with  field sample date=2013-10-22, even though some are in europe
  dplyr::select(Facility_Short_Name,SPECIES_Code,ORIGIN_TYPE_Code,Lat,Lon)
salmontab<-rbind(wildtab,hatchtab)
salmontab<-salmontab[order(salmontab$Facility_Short_Name,salmontab$SPECIES_Code),]
#Remove sites outside of puget sound proper
salmontab<-salmontab[!salmontab$Facility_Short_Name=="GLENWOOD SPRINGS",]
salmontab<-salmontab[!salmontab$Facility_Short_Name=="WHATCOM CR HATCHERY",]
salmontab<-salmontab[!salmontab$Facility_Short_Name=="ELWHA HATCHERY",]

write.csv(salmontab,"analyses/output/salmonlatlon.csv", row.names = FALSE)
types<-c("wild","hatch")
sp<-site<-type<-firstcoefsall<-lastcoefsall<-midcoefsall<-peakcoefsall<-meantotalall<-c()
sp2<-site2<-type2<-yearsall<-firstobsdateall<-lastobsdateall<-alltotalall<-peakobsdateall<-midobsdateall<-c()
for(w in 1:2){#type
  if(w==1){d2<-wild.d}
  if(w==2){d2<-hatch.d}
  

allyears<-unique(d2$BROOD_Yr)
allyears<-sort(allyears[allyears<2019])
#restrict to summer/fall/winter runs
d2<-d2[d2$doy>152,]#June 1 or later
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
  
  
  #save all phendats across years to make a BIG table
  sp2<-c(sp2,rep(species[p],times=length(year)))
  site2<-c(site2, rep(as.character(sites[i]),times=length(year)))
  type2<-c(type2,rep(types[w],times=length(year)))
  yearsall<-c(yearsall,year)
  firstobsdateall<-c(firstobsdateall,firstobsdate)
  lastobsdateall<-c(lastobsdateall,lastobsdate)
  alltotalall<-c(alltotalall,alltotal)
  peakobsdateall<-c(peakobsdateall,peakobsdate)
  midobsdateall<-c(midobsdateall,midobsdate)
  }
}

#make a big table of model output
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


#make a big table of phendates
allphen<-as.data.frame(cbind(type2,sp2,site2,yearsall,alltotalall,firstobsdateall,lastobsdateall,
                             midobsdateall, peakobsdateall))

allphen <- allphen[apply(allphen, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
dim(allphen)

allphen$group<-paste(allphen$site2,allphen$sp2,allphen$type, sep=".")#this will be grouping factor/random effect
allphen$group<-as.factor(allphen$group)
unique(allphen$group)
allphen$type2<-as.factor(allphen$type2)
allphen$sp2<-as.factor(allphen$sp2)

allphen$yearsall<-as.numeric(allphen$yearsall)
allphen$firstobsdateall<-as.integer(allphen$firstobsdateall)
allphen$lastobsdateall<-as.numeric(allphen$lastobsdateall)
allphen$peakobsdateall<-as.numeric(allphen$peakobsdateall)
allphen$midobsdateall<-as.numeric(allphen$midobsdateall)
allphen$yearsall<-as.integer(allphen$yearsall)

firstmod<-lmer(firstobsdateall~yearsall+(1|group), data=allphen, REML=FALSE)
#firstmod2<-lmer(firstobsdateall~yearsall+(yearsall|group), data=allphen, REML=FALSE)

lastmod<-lmer(lastobsdateall~yearsall+(1|group), data=allphen, REML=FALSE)
#lastmod2<-lmer(lastobsdateall~yearsall+(yearsall|group), data=allphen, REML=FALSE)

peakmod<-lmer(peakobsdateall~yearsall+(1|group), data=allphen, REML=FALSE)
#peakmod2<-lmer(peakobsdateall~yearsall+(yearsall|group), data=allphen, REML=FALSE)

midmod<-lmer(midobsdateall~yearsall+(1|group), data=allphen, REML=FALSE)
#midmod2<-lmer(midobsdateall~yearsall+(yearsall|group), data=allphen, REML=FALSE)
#summary(peakmod2)
#make a big table of model output
allmodsums<-as.data.frame(cbind(c(fixef(firstmod)[1],confint(firstmod, level = 0.50)[3,1:2],confint(firstmod, level = 0.75)[3,1:2],confint(firstmod, level = 0.95)[3,1:2]),
                                  c(fixef(firstmod)[2],confint(firstmod, level = 0.50)[4,1:2],confint(firstmod, level = 0.75)[4,1:2],confint(firstmod, level = 0.95)[4,1:2]),
                                 c(fixef(peakmod)[1], confint(peakmod,level=0.5)[3,1:2],confint(peakmod,level=0.75)[3,1:2],confint(peakmod,level=0.95)[3,1:2]),
                                c(fixef(peakmod)[2], confint(peakmod,level=0.5)[4,1:2],confint(peakmod,level=0.75)[4,1:2],confint(peakmod,level=0.95)[4,1:2]),
                              c(fixef(lastmod)[1],confint(lastmod, level = 0.50)[3,1:2],confint(lastmod, level = 0.75)[3,1:2],confint(lastmod, level = 0.95)[3,1:2]),
                                c(fixef(lastmod)[2],confint(lastmod, level = 0.50)[4,1:2],confint(lastmod, level = 0.75)[4,1:2],confint(lastmod, level = 0.95)[4,1:2])))


colnames(allmodsums)<-c("first.int","first.yr","pk.int","pk.yr","last.int","last.yr")
rownames(allmodsums)<-c("mean","perc25","perc75","perc125","perc875","perc05","perc975")
allmodsums<-round(allmodsums,digits=3)
write.csv(allmodsums, "analyses/output/salmonreturntrends_pslmm.csv", row.names = TRUE)



#Look at effect 