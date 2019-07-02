# Orca analysis: occupancy models
# Started November 1327, 2018
# by Ailene Ettinger ailene.ettinger@noo.gov
#NEed to fix this
!
#housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/fishphen")

# Load libraries
library(dplyr)
# 1. Get the data
d <- read.csv("data/AppendixII.csv")

# 2. Clean the data (also saved in output/AppendixII_cleaned,csv)
source("analyses/orcaphen/source/clean_orca.R")

#Create a new column that combines Pod and Likely Pod columna and removes spaces
d$Pod.cl<-d$Pod


#Always use Likely Pod column, when it is not blank:
d$Pod.cl[d$LikelyPod!="" & d$LikelyPod!=" "]<-d$LikelyPod[d$LikelyPod!="" & d$LikelyPod!=" "]
#perhaps also stick with Pod when LikelyPod has a "?" grep("?",d$LikelyPod,)

#remove non-orca data
#d<-d[d$Pod.cl=="HB?"|d$Pod.cl=="Not Orcas",]

#only using fishing areas in Washington's Salish Sea 
#d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584
#keep canadian sites in upper salish sea
d<-d[d$FishArea %in% c("01","02","03","04","05","06","07","09","10","11","12","13","81","82","19C","18C","29C","20C"),]#not sure where 17, 18, 19, 20, 28, 29 are...need to find out. also, where is 42583,42584
#remove sites with no fishing area:
d<-d[!d$FishArea %in% c(""),]


#Only use fishing areas that have atleast 4 years with >20 observations:
#11 fishing areas with >5
# 8 fishing areas with >10
# 8 fishing areas with >20
tab.fa.yr<-table(d$FishArea,d$Year)
tab.fa.yr[tab.fa.yr < 20] <- 0
tab.fa.yr[tab.fa.yr >= 20] <- 1
sites.touse<-rownames(tab.fa.yr)[rowSums(tab.fa.yr)>4]
d<-d[d$FishArea %in% c(sites.touse),]

#Add week and day of year (day)
d$day<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"),format= "%j")
d$week<-strftime(strptime(paste(d$Month, d$Day, d$Year, sep="."),format= "%m.%d.%Y"), format = "%V")#new weeks start on mondays

#Add a column for presences (1/0) for each pod, for Ts, and for SRKWs

d$J<-0
d$J[grep("J",d$Pod.cl)]<- 1
d$K<-0
d$K[grep("K",d$Pod.cl)]<- 1
d$L<-0
d$L[grep("L",d$Pod.cl)]<- 1
d$SRKW<-0
d$SRKW[grep("SR",d$Pod.cl)]<- 1
d$SRKW[d$J==1|d$K==1|d$L==1]<- 1   
d$Orcas<-1

#only data after 1978
d<-d[d$Year>1978,]

#Prepare data for running model from strebel et al 2014
#"nrep" "ndet" "site" "day" "year"

# Add a column for total number of observations 
#of all SRKWs during each week of each year year to estimate detection
#d$wkyrfa<-paste(d$Year, d$week,d$FishArea,sep="_")#if we decide to add FishArea to detection estimates for model
#d$wkyr<-paste(d$Year, d$week,sep="_")
#unique(d$FishArea)
d$yrdayfa<-paste(d$Year, d$day,d$FishArea,sep="_")

#Raw detection ratios:
obs = aggregate(Orcas ~yrdayfa, data = d,sum)

# presence of each pod
js = aggregate(J ~yrdayfa, data = d,sum)
ks = aggregate(K~yrdayfa, data = d,sum)
ls = aggregate(L~yrdayfa, data = d,sum)
srs = aggregate(SRKW~yrdayfa, data = d,sum)

det<-cbind(js,ks[,2],ls[,2],srs[,2],obs[2])
colnames(det)[2:6]<-c("Jobs","Kobs","Lobs","AllSRobs","nrep")

det$year<-substr(det$yrdayfa,1,4)
det$day<-substr(det$yrdayfa,6,8)
det$fa<-substr(det$yrdayfa,10,nchar(det$yrdayfa))
#assign to ps (puget sound) or uss (upper salish sea) using fishing area
det$region<-"ps"
det$region[det$fa=="07"|det$fa=="06"|det$fa=="05"|det$fa=="04"|det$fa=="19C"|det$fa== "18C"|det$fa=="20C"]<-"uss"
det$region[det$fa=="01"|det$fa=="02"|det$fa=="03"]<-"oc"#outer coast

det$site<-as.numeric(as.factor(det$fa))
det$day<-as.numeric(det$day)
det$year<-as.numeric(det$year)

#Add a column for "season" and divide up by season.Not sure if these are best...
#start with winter vs summer
det$season<-NA
#det$season[det$day>274|det$day<60]<-1#winter (Oct 1-March)
det$season[det$day>274]<-1#winter (Oct 1-Dec 31)

det$season[det$day>121 & det$day<274]<-2#summer (May-Sept 31)
#add an "orca year" which runs Oct 1-Sept 31
#det$orcayear<-det$year
#det$orcayear[which(det$day>273)]<-det$year[which(det$day>273)]+1
#det$daysaftsept30<-NA
#det$daysaftsept30[which(det$day>273 & det$day<367)]<-det$day[which(det$day>273 & det$day<367)]-273
#det$daysaftsept30[which(det$day<274)]<-det$day[which(det$day<274)]+93#this should actually vary depending on whether or not it is a leap year
#jdet<-subset(det,select=c(nrep,Jobs,site,day,orcayear,daysaftsept30,season,region))
jdet<-subset(det,select=c(nrep,Jobs,site,day,year,day,season,region))
#i.e. use only sites that have atleast 1 observation in all years

jdet <- jdet[apply(jdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#jdet<- jdet[apply(jdet,1,function(row) all(row!=0)),]
#kdet<-subset(det,select=c(nrep,Kobs,site,day,orcayear,daysaftsept30,season,region))
kdet<-subset(det,select=c(nrep,Kobs,site,day,year,day,season,region))

kdet <- kdet[apply(kdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#ldet<-subset(det,select=c(nrep,Lobs,site,day,orcayear,daysaftsept30,season,region))
ldet<-subset(det,select=c(nrep,Lobs,site,day,year,day,season,region))
ldet <- ldet[apply(ldet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na
#srdet<-subset(det,select=c(nrep,AllSRobs,site,day,orcayear,daysaftsept30,season,region))
srdet<-subset(det,select=c(nrep,AllSRobs,site,day,year,day,season,region))
srdet <- srdet[apply(srdet, 1, function(x) all(!is.na(x))),] # only keep rows of all not na

colnames(srdet)[2]<-colnames(jdet)[2]<-colnames(kdet)[2]<-colnames(ldet)[2]<-"ndet"
colnames(srdet)[5]<-colnames(jdet)[5]<-colnames(kdet)[5]<-colnames(ldet)[5]<-"year"


write.csv(srdet,"analyses/output/allsr_dat.csv",row.names = FALSE)
write.csv(ldet,"analyses/output/l_dat.csv",row.names = FALSE)
write.csv(kdet,"analyses/output/k_dat.csv",row.names = FALSE)
write.csv(jdet,"analyses/output/j_dat.csv",row.names = FALSE)

#Make some plots to look at the data
#summarize by sitings per day of each season and pod over time
# Choose the data you want to look at:
pods<-c("J","K","L","SR")
seasons<-c(1,2)
for(i in 1:length(pods)){
  for(j in 1:length(seasons)){
    
  pod=pods[i]#options= J,K,L, SR (all 3 pods)
  season=seasons[j]#options= 1(winter) or 2(summer)
  if(season==1){region="ps"}#options=upper salish sea (uss) or puget sound (ps)
  if(season==2){region="uss"}

  if(pod=="J"){dat<-jdet}
  if(pod=="K"){dat<-kdet}
  if(pod=="L"){dat<-ldet}
  if(pod=="SR"){dat<-srdet}

  #restrict to season
  dat<-dat[dat$season==season,]

  #if winter  (season 1), then days= days ater sept 30
  if(season=="1"){
  dat<-subset(dat,select=c(nrep,ndet,site, daysaftsept30,year,season,region))
  colnames(dat)[4]<-"day"
  }

  #choose region
  dat<-dat[dat$region==region,]

  dim(dat)
  #find doy with max number of obs for each year

  dayobs<-aggregate(dat$ndet,list(dat$year,dat$day),sum)
  colnames(dayobs)<-c("year","day","obs")

  dayeffort<-aggregate(dat$nrep,list(dat$year,dat$day),sum)
  colnames(dayeffort)<-c("year","day","effort")

  propobs<-left_join(dayobs,dayeffort)
  propobs$propobs<-propobs$obs/propobs$effort

  #find which day this occurs on
  maxobs<-aggregate(dayobs$obs,list(dayobs$year),max)
  colnames(maxobs)<-c("year","obs")
  peakobs<-dplyr::inner_join(maxobs, dayobs)

  maxprop<-aggregate(propobs$propobs,list(propobs$year),max)
  colnames(maxprop)<-c("year","propobs")
  peakpropdoy<-dplyr::inner_join(maxprop,propobs)

  #plot peak obs by year
  pdf(file=paste("analyses/figures/SR/orcaphen_1976_2017",region,season,pod,"rawdat.pdf", sep="_"),width=7,height=6)

  ### plot peak number of obs over all years
  #quartz()
  par(mfrow=c(1,1),mai=c(1,1,1,0.5))
  x=peakobs$year
  y=peakobs$day
  plot(x,y,xlab="Year",ylab="DOY of peak # sightings",main=paste("Peak Number of Sightings","\n",pod[i]," Pod",season[j]),
     ylim=c(min(peakobs$day),max(peakobs$day)),pch=21,type="p", bg="gray")
  dev.off()
  print(pods[i]);  print(seasons[j]);
  print(summary(lm(y~x)))
  print(summary(lmer(y~x+(1|as.factor(x)))))

  # plot peak proportion of obs over all years
  #quartz()
  pdf(file=paste("analyses/figures/SR/orcaphen_1976_2017",region,season,pod,"prop.pdf", sep="_"),width=7,height=6)
  
  par(mfrow=c(1,1),mai=c(1,1,1,0.5))
  x=peakpropdoy$year
  y=peakpropdoy$day
  plot(x,y,xlab="Year",ylab="DOY of peak proportion sightings",main=paste("Peak Proportion of Sightings","\n",pod[i]," Pod",season[j]),
     ylim=c(min(peakpropdoy$day),max(peakpropdoy$day)),pch=21,type="p", bg="gray")
  dev.off()
  print(pods[i]);print(seasons[j])
  print(summary(lm(y~x)))
  print(summary(lmer(y~x+(1|as.factor(x)))))
  
  years<-unique(dat$year)
  for(yr in min(years):max(years)){
    #pdf(file=paste("analyses/figures/", pod,"/rawdat",yr,"_",season,"_",region,".pdf", sep=""),width=8,height=6)
    yrdat<-dat[dat$year==yr,]
    x=yrdat$day
    y=yrdat$ndet
    quartz()
    plot(x,y,main=paste("Number sightings","\n",pod," Pod",yr),
         ylim=c(min(yrdat$nrep, na.rm = TRUE),max(yrdat$nrep, na.rm = TRUE)),pch=16,type="p", col="black")
    #dev.off()
  }
  
  
}
}

#Now make figures for whole year
# Choose the data you want to look at:
pods<-c("J","K","L","SR")
for(i in 1:length(pods)){
    pod=pods[i]
    if(pod=="J"){dat<-jdet}
    if(pod=="K"){dat<-kdet}
    if(pod=="L"){dat<-ldet}
    if(pod=="SR"){dat<-srdet}
    
    #find doy with max number of obs for each year
    
    dayobs<-aggregate(dat$ndet,list(dat$year,dat$day),sum)
    colnames(dayobs)<-c("year","day","obs")
  
    years<-unique(dayobs$year)
    for(yr in min(years):max(years)){
      pdf(file=paste("analyses/figures/", pod,"/rawdat",yr,"_yearroundallsites_",".pdf", sep=""),width=8,height=6)
      yrdat<-dayobs[dayobs$year==yr,]
      x=yrdat$day
      y=yrdat$obs
      plot(x,y,main=paste("Number sightings","\n",pod," Pod",yr),
           ylim=c(min(yrdat$nrep, na.rm = TRUE),max(yrdat$nrep, na.rm = TRUE)),xlab="doy",ylab="# sightings",pch=16,type="p", col="black")
      dev.off()
    }
    
    
  }
}




